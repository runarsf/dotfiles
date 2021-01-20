#!/bin/sh

killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 1; done

CONFIG="${HOME}/.config/polybar/config.ini"
BARS=("main" "secondary")

if test -z "$(pgrep -x polybar)"; then
  for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
    #IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
    #if [[ $IS_PRIMARY == *"primary"* ]]; then
    #  TRAY_POSITION=right
    #else
    #  TRAY_POSITION=none
    #fi
    for BAR in "${BARS[@]}"; do
      printf "Launching bar: ${BAR}\n"
      TRAY_POSITION="${TRAY_POSITION}" MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload "${BAR}" &
    done
  done
else
  polybar-msg cmd restart
fi
