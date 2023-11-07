#!/bin/sh

if test "$(hyprctl -j activewindow | jq '.floating')" != "true"; then
  hyprctl dispatch togglefloating
fi

hyprctl dispatch pin
