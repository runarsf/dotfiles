#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash socat jq

# Implements a pseudo-monocle mode for Hyprland by handling events
# and adjusting window states accordingly.

set -o errexit
set -o nounset
set -o pipefail

monocle="false"

hyprctl keyword master:inherit_fullscreen true >/dev/null

handle () {
  event="$(printf '%s' "${1}" | awk -F'>>' '{print $1}')"

  case "${event}" in
    focusedmon|workspacev2|fullscreen|openwindow)
      fullscreen="$(hyprctl -j activewindow | jq -r '(.fullscreen|tostring) + "-" + (.fullscreenMode|tostring)')"
      if test "${fullscreen}" = "true-1"; then
        monocle="true"
      else
        monocle="false"
      fi
      ;;&

    openwindow|closewindow)
      address="$(printf '%s' "${1}" | awk -F'>>|,' '{print $2}')"
      ;;&

    openwindow)
      if test "${monocle}" = "true"; then
        hyprctl dispatch focuswindow "address:0x${address}" >/dev/null
      fi
      ;;

    closewindow)
      clients="$(hyprctl -j activeworkspace | jq -r '.windows')"
      fullscreen="$(hyprctl -j activewindow | jq -r '.fullscreen')"
      if test "${monocle}" = "true" -a "${clients}" -gt "1" -a "${fullscreen}" != "true"; then
        hyprctl dispatch fullscreen 1 >/dev/null
      fi
      ;;
  esac
}

handle focusedmon
socat -u "UNIX-CONNECT:/tmp/hypr/${HYPRLAND_INSTANCE_SIGNATURE}/.socket2.sock" - | while read -r line; do
  handle "${line}"
done

