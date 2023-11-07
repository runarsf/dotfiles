#!/usr/bin/env bash

# Automatically moves the window to the nearest edge of the monitor and resizes it to fit the screen.

window="$(hyprctl -j activewindow)"
monitor="$(hyprctl -j monitors | jq -r '.[] | select(.focused)')"

gaps="$(hyprctl -j getoption general:gaps_out | jq -r '.int')"
border="$(hyprctl -j getoption general:border_size | jq -r '.int')"

pad_n="$((${gaps} + ${border} + $(hyprctl -j monitors | jq -r '.[] | select(.focused) | .reserved[1]')))"
pad_s="$((${gaps} + ${border} + $(hyprctl -j monitors | jq -r '.[] | select(.focused) | .reserved[3]')))"
pad_w="$((${gaps} + ${border} + $(hyprctl -j monitors | jq -r '.[] | select(.focused) | .reserved[0]')))"
pad_e="$((${gaps} + ${border} + $(hyprctl -j monitors | jq -r '.[] | select(.focused) | .reserved[2]')))"

pid="$(printf '%s\n' "${window}" | jq -r '.pid')"

x="$(printf '%s\n' "${window}" | jq -r '.at[0]')"
y="$(printf '%s\n' "${window}" | jq -r '.at[1]')"
w="$(printf '%s\n' "${window}" | jq -r '.size[0]')"
h="$(printf '%s\n' "${window}" | jq -r '.size[1]')"

old_x="${x}"
old_y="${y}"
old_w="${w}"
old_h="${h}"

mon_x="$(printf '%s\n' "${monitor}" | jq -r '.x')"
mon_y="$(printf '%s\n' "${monitor}" | jq -r '.y')"
mon_w="$(printf '%s\n' "${monitor}" | jq -r '.width')"
mon_h="$(printf '%s\n' "${monitor}" | jq -r '.height')"

max_w="$((${mon_w} - ${pad_w} - ${pad_e}))"
max_h="$((${mon_h} - ${pad_n} - ${pad_s}))"

test "${w}" -gt "${max_w}" && w="${max_w}"
test "${h}" -gt "${max_h}" && h="${max_h}"

max_x="$((${mon_w} - ${w} - ${pad_s}))"
max_y="$((${mon_h} - ${h} - ${pad_e}))"
min_x="$((${mon_x} + ${pad_w}))"
min_y="$((${mon_y} + ${pad_n}))"

test "${x}" -gt "${max_x}" && x="${max_x}"
test "${y}" -gt "${max_y}" && y="${max_y}"
test "${x}" -lt "${min_x}" && x="${min_x}"
test "${y}" -lt "${min_y}" && y="${min_y}"

if test "${x}" -ne "${old_x}" -o "${y}" -ne "${old_y}"; then
  hyprctl dispatch movewindowpixel exact "${x}" "${y}",pid:${pid}
fi
if test "${w}" -ne "${old_w}" -o "${h}" -ne "${old_h}"; then
  hyprctl dispatch resizewindowpixel exact "${w}" "${h}",pid:${pid}
fi
