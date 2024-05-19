#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

direction="${1:?}"

fullscreen="$(hyprctl -j activewindow | jq -r '(.fullscreen|tostring) + "-" + (.fullscreenMode|tostring)')"

if test "${fullscreen}" = "true-1"; then
  case "${direction}" in
    l|u) hyprctl dispatch cyclenext prev;;
    r|d) hyprctl dispatch cyclenext;;
  esac
else
  hyprctl --batch "dispatch movefocus ${direction}; dispatch movecursortocorner 2"
fi
