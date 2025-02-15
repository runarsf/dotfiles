#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-output-monitor nvd nh
set -o nounset
set -o errexit
set -o pipefail

RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
NORMAL="$(tput sgr0)"

# Add default arguments if none are provided
if ! printf '%s\n' "${@}" | grep -Fxqz -- '--' \
  && test "${#}" -ge "2"; then
  set -- "${@}" "--" "--accept-flake-config"
fi

nh "${@}"

BIN="$(basename "${0}")"
COMMAND="${1}"

confirm () {
  message="${1:-Continue?}"
  prompt=${2:+Y/n}
  prompt=${PROMPT:-y/N}

  printf '%s (%s)%s ' "${message}" "${prompt}" "${NORMAL}"
  read -r choice

  case "${choice}" in
    [nN][oO]|n|N|"${2:+n}") return 1;;
    [yY][eE][sS]|y|Y|"") return 0;;
    *) printf 'Invalid choice: %s\n' "${choice}"; "${FUNCNAME[1]}" "${@}";;
  esac
}

while test "${#}" -gt "0"; do
  case "${COMMAND}::${1}" in
    os::switch)
      # Only attempt if sudo is already cached
      if sudo -n true 2>/dev/null; then
        # Only warn when more than 10 generations exist
        generations="$(sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | wc -l)"
        if test "${generations}" -ge "10"; then
          if confirm "${YELLOW}You currently have ${RED}${generations} generations${YELLOW}, want to clean up now?"; then
            "${0}" clean all --keep=3
          else
            printf '\nAlright :⁽ Consider cleaning up by running:\n'
            printf '  $ %s clean all --keep=3\n\n' "${BIN}"
	  fi
        fi
      fi;;
    home::switch)
      # Only warn when more than 10 generations exist
      generations="$(home-manager generations | wc -l)"
      if test "${generations}" -ge "10"; then
        if confirm "${YELLOW}You currently have ${RED}${generations} generations${YELLOW}, want to clean up now?"; then
          "${0}" clean user --keep=3
        else
          printf '\nAlright :⁽ Consider cleaning up by running:\n'
          printf '  $ %s clean user --keep=3\n\n' "${BIN}"
        fi
      fi;;
    *::test)
      printf '\n%sBuild success! Remember to switch if everything looks good ;⁾%s\n' "${GREEN}" "${NORMAL}"
      printf '  $ %s %s switch\n' "${BIN}" "${COMMAND}";;
  esac

  shift
done
