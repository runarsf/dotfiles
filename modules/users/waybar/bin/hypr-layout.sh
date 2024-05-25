#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash socat jq

set -o errexit
set -o nounset
set -o pipefail

handle () {
  sleep 0.1
  event="$(printf '%s' "${1}" | awk -F'>>' '{print $1}')"

  case "${event}" in
    focusedmon|workspacev2|fullscreen|openwindow|closewindow) :;;
    *) return;;
  esac

  monocle="$(hyprctl -j activeworkspace | jq -r '.hasfullscreen')"
  # FIXME These gets the value set in the config, not the active ws
  layout="$(hyprctl -j getoption general:layout | jq -r '.str')"
  orientation="$(hyprctl -j getoption master:orientation | jq -r '.str')"

  if test "${layout}" = "master" -a "${monocle}" = "true"; then
    layout="monocle"
  elif test "${layout}" = "master"; then
    layout="${orientation}"
  fi

  case "${layout}" in
    left)    printf '[]=\n';;
    center)  printf '|[]|\n';;
    monocle) printf '[M]\n';;
    dwindle) printf 'BSP\n';;
  esac
}

handle focusedmon
socat -u "UNIX-CONNECT:/tmp/hypr/${HYPRLAND_INSTANCE_SIGNATURE}/.socket2.sock" - | while read -r line; do
  handle "${line}"
done

