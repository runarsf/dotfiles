#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

action="${1:?}"
direction="${2:?}"

fullscreen="$(hyprctl -j activeworkspace | jq -r '.hasfullscreen')"

if test "${fullscreen}" = "true"; then
  case "${action}-${direction}" in
    # The master layout has the layoutmsgs cyclenext and cycleprev,
    # but they're not supported in dwindle, so we don't use them.
    focus-l|focus-u) hyprctl dispatch cyclenext prev hist;;
    focus-r|focus-d) hyprctl dispatch cyclenext hist;;
    window-l|window-u) hyprctl dispatch swapnext prev;;
    window-r|window-d) hyprctl dispatch swapnext;;
  esac
else
  if test "${action}" = "focus"; then
    hyprctl --batch "dispatch movefocus ${direction}; dispatch movecursortocorner 2"
  elif test "${action}" = "window"; then
    hyprctl dispatch movewindow "${direction}"
  fi
fi
