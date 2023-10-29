#!/usr/bin/env sh

if test "$(hyprctl -j getoption general:layout | jq -r '.str')" = "master"; then
  layout="dwindle"
else
  layout="master"
fi

printf 'Switching to %s\n' "${layout}"
hyprctl keyword general:layout "${layout}"
