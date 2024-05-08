#! /usr/bin/env cached-nix-shell
#! nix-shell -i bash -p nix-output-monitor nvd nh
set -o nounset
set -o errexit
set -o pipefail

nh "${@}"

COMMAND="${1}"

while test "${#}" -gt "0"; do
  case "${COMMAND}-${1}" in
    os-switch)
      if sudo -n true 2>/dev/null; then
        generations="$(sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | wc -l)"
        if test "${generations}" -gt "10"; then
          printf '\nYou currently have %s generations, consider cleaning them up:\n' "${generations}"
          printf '$ sudo nix-collect-garbage -d\n'
          printf '$ nh os switch\n'
        fi
      fi;;
    os-test)
      printf '\nBuild success! Remember to switch if everything looks good ;⁾\n'
      printf '$ nh os switch\n'
      ;;
    home-switch)
      generations="$(home-manager generations | wc -l)"
      if test "${generations}" -gt "10"; then
        printf '\nYou currently have %s generations, consider cleaning them up:\n' "${generations}"
        printf '$ nh clean all --keep=5\n'
        printf 'OR\n'
        printf '$ nix-collect-garbage -d\n'
      fi;;
  esac

  shift
done
