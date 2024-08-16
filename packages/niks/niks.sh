#! /usr/bin/env cached-nix-shell
#! nix-shell -i bash -p nix-output-monitor nvd nh
set -o nounset
set -o errexit
set -o pipefail

nh "${@}"

bin="$(basename "${0}")"
COMMAND="${1}"

while test "${#}" -gt "0"; do
  case "${COMMAND}-${1}" in
    os-switch)
      # Only attempt if sudo is already cached
      if sudo -n true 2>/dev/null; then
        generations="$(sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | wc -l)"
        if test "${generations}" -gt "10"; then
          printf '\nYou currently have %s generations, consider cleaning up your system:\n' "${generations}"
          printf '$ sudo nix-collect-garbage -d\n'
          printf '$ nix-collect-garbage -d\n'
          printf '\n$ %s os switch\n' "${bin}"
        fi
      fi;;
    os-test)
      printf '\nBuild success! Remember to switch if everything looks good ;⁾\n'
      printf '$ %s os switch\n' "${bin}";;
    home-switch)
      generations="$(home-manager generations | wc -l)"
      if test "${generations}" -gt "10"; then
        printf '\n\033[1;33mYou currently have \033[0;31m%s generations\033[1;33m, consider cleaning them up:\n' "${generations}"
        printf '   $ %s clean all --keep=5\n' "${bin}"
        printf 'or $ nix-collect-garbage -d\n'
      fi;;
  esac

  shift
done
