#!/bin/sh

# CONFIG=${1}; shift
# Test if config exists in ~/.config/polybar/${CONFIG}, if not then test if the absolute path exists

killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 1; done

CONFIG="${HOME}/.config/polybar/config.ini"
BARS=("main" "secondary")

if test -z "$(pgrep -x polybar)"; then
  for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
    IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
    if [[ $IS_PRIMARY == *"primary"* ]]; then
      TRAY=right
    else
      TRAY=none
    fi
    for BAR in "${BARS[@]}"; do
      printf "Launching bar: ${BAR}\n"
      TRAY="${TRAY}" MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload "${BAR}" &
    done
  done
else
  polybar-msg cmd restart
fi
