#!/usr/bin/env bash

# Automatically moves the window to the nearest edge of the monitor.

gaps="$(hyprctl -j getoption general:gaps_out | jq -r '.int')"
border="$(hyprctl -j getoption general:border_size | jq -r '.int')"
padding="$((gaps + border))"

window="$(hyprctl -j activewindow)"
monitor="$(hyprctl -j monitors | jq -r '.[] | select(.focused)')"

pid="$(printf '%s\n' "${window}" | jq -r '.pid')"

x="$(printf '%s\n' "${window}" | jq -r '.at[0]')"
y="$(printf '%s\n' "${window}" | jq -r '.at[1]')"
w="$(printf '%s\n' "${window}" | jq -r '.size[0]')"
h="$(printf '%s\n' "${window}" | jq -r '.size[1]')"

mon_x="$(printf '%s\n' "${monitor}" | jq -r '.x')"
mon_y="$(printf '%s\n' "${monitor}" | jq -r '.y')"
mon_w="$(printf '%s\n' "${monitor}" | jq -r '.width')"
mon_h="$(printf '%s\n' "${monitor}" | jq -r '.height')"

min_x="$((${mon_x} + ${padding}))"
min_y="$((${mon_y} + ${padding}))"
max_x="$((${mon_w} - ${w} - ${padding}))"
max_y="$((${mon_h} - ${h} - ${padding}))"

new_x="${x}"
new_y="${y}"

test "${x}" -lt "${min_x}" && new_x="${min_x}"
test "${y}" -lt "${min_y}" && new_y="${min_y}"
test "${x}" -gt "${max_x}" && new_x="${max_x}"
test "${y}" -gt "${max_y}" && new_y="${max_y}"

hyprctl dispatch movewindowpixel exact "${new_x}" "${new_y}",pid:${pid}
