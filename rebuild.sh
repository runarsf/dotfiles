#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash jq nix-output-monitor home-manager

set -o errexit
set -o nounset
set -o pipefail
set -o posix
shopt -s extglob
shopt -s extdebug

# Convenience wrapper around nix rebuild commands.

# Since the script is copied to the nix store when adding to ~/.local/bin,
# we sadly need to hard code the path so we can know where the flake resides.
: "${SCRIPTPATH:=${HOME}/.config/nixos}"
pushd "${SCRIPTPATH}" >/dev/null

# The builder to use, defined as build__<builder>
BUILDER="nixos"
# The flake to build
FLAKE=""
# This will be passed as shell parameters to the build command
OVERFLOW=("test")

debug () {
  # https://stackoverflow.com/a/62012691
  # Print a '+' for every element in BASH_LINENO, similar to PS4's behavior
  printf '%s' "${BASH_LINENO[@]/*/+}" >&2
  # Then print the current command, colored
  printf ' \e[33m%s\e[0m\n' "${BASH_COMMAND}" >&2
}

parse_args () {
  # TODO If systems is empty and there are more than one command,
  #  the first command should be set as OVERFLOW[1], the second s FLAKE, and the rest appended to OVERFLOW.
  # TODO Find a way of viewing homeConfigurations, `nix flake show` for some
  #  reason doesn't seem to expose this property correctly.
  #  Currently --flake is a workaround to set a different home-manager flake.

  # Get a list of nixos configurations exported by the flake
  readarray -t systems < <(nix --extra-experimental-features 'nix-command flakes' flake show --json 2>/dev/null | jq -rc '.nixosConfigurations | keys | .[]')

  while [ "${#}" -gt "0" ]; do
    case "${1}" in
      os|nixos|system)
        BUILDER="nixos"
        shift;;
      home|hm|home-manager)
        BUILDER="home_manager"
        shift;;
      -f|--flake) # <name>
        FLAKE="${2}"
        shift;shift;;
      # Match a dash followed by two or more characters
      -??*)
        # Nix's tooling doesn't handle chained short options,
        # split them up into separate parts for better option parsing.
        local opt="${1}"
        shift # Discard "-xyz" and prepend "-x -yz"
        set -- "${opt:0:2}" "-${opt:2}" "${@}";;
      # Match any string not containing dashes (not an option -> subcommand)
      !(*-*))
        # Check if the subcommand is exported as a nixos configuration from the flake
        if printf '%s\0' "${systems[@]}" | grep -Fxqz -- "${1}"; then
          FLAKE="${1}"
        else
          OVERFLOW[0]="${1}"
        fi
        shift;;
      *)
        OVERFLOW+=("${1}")
        shift;;
    esac
  done
}

build__nixos () {
  sudo true # ensure sudo is cached, nom will eat the password prompt
  sudo nixos-rebuild \
       --verbose \
       --log-format 'internal-json' \
       --flake ".#${FLAKE}" \
       ${@} \
    |& nom --json
}

build__home_manager () {
  home-manager \
    --verbose \
    --extra-experimental-features 'nix-command flakes' \
    --flake ".#${FLAKE}" \
    ${@}
}

main () {
  parse_args "${@}"

  if ! declare -f -F "build__${BUILDER}" > /dev/null; then
    printf 'Unknown builder: %s\n' "${BUILDER}" >&2
    exit 1
  fi

  trap debug DEBUG
  "build__${BUILDER}" "${OVERFLOW[@]}"
  trap - DEBUG

  case "${OVERFLOW[0]}" in
    build)
      # Prompt user to switch to the built configuration
      read -p 'Build success, switch now? [y/N] ' -n 1 -r
      printf '\n'
      if [[ $REPLY =~ ^[Yy]$ ]]; then
        OVERFLOW[0]="switch"
        main "${OVERFLOW[@]}"
      fi;;
    test)
      printf 'Build success! Remember to switch if everything looks good ;⁾\n'
      printf '$ %s switch %s\n' "$(basename "${0}")" "${OVERFLOW[@]:1}";;
    switch)
      printf 'Switched to new configuration! :⁾\n'
      if sudo -n true 2>/dev/null; then
        local generations="$(sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | wc -l)"
        if test "${generations}" -gt "10"; then
          printf 'You currently have %s generations, consider cleaning them up:\n' "${generations}"
          printf '$ sudo nix-collect-garbage -d\n'
          printf '$ %s switch %s\n' "$(basename "${0}")" "${OVERFLOW[@]:1}"
        fi
      fi;;
  esac
}

main "${@}"
exit "${?}"
