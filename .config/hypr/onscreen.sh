#!/usr/bin/env bash

# Automatically moves the window to the nearest edge of the monitor and resizes it to fit the screen.

window="$(hyprctl -j activewindow)"
monitor="$(hyprctl -j monitors | jq -r '.[] | select(.focused)')"

gaps="$(hyprctl -j getoption general:gaps_out | jq -r '.int')"
border="$(hyprctl -j getoption general:border_size | jq -r '.int')"
padding="$((gaps + border))"

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
max_w="$((${mon_w} - ${padding} - ${padding}))"
max_h="$((${mon_h} - ${padding} - ${padding}))"

new_x="${x}"
new_y="${y}"
new_w="${w}"
new_h="${h}"

test "${x}" -lt "${min_x}" && new_x="${min_x}"
test "${y}" -lt "${min_y}" && new_y="${min_y}"
test "${x}" -gt "${max_x}" && new_x="${max_x}"
test "${y}" -gt "${max_y}" && new_y="${max_y}"
test "${w}" -gt "${max_w}" && new_w="${max_w}"
test "${h}" -gt "${max_h}" && new_h="${max_h}"

hyprctl dispatch resizewindowpixel exact "${new_w}" "${new_h}",pid:${pid}
hyprctl dispatch movewindowpixel exact "${new_x}" "${new_y}",pid:${pid}
