#!/bin/sh

CONFIG="${HOME}/.config/polybar/config"

killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 1; done

if test -z "$(pgrep -x polybar)"; then
  for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
    printf "Launching bar: ${bar}\n"
    IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
    if [[ $IS_PRIMARY == *"primary"* ]]; then
      # https://manpages.ubuntu.com/manpages/bionic/man1/trayer.1.html
      MONITOR="${MONITOR}" WIDTH="92%" polybar --config="${CONFIG}" --reload main &
      MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload tray &
    else
      MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload main &
    fi
  done
else
  polybar-msg cmd restart
fi

#if test -z "$(pgrep -x polybar)"; then
#  for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
#    IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
#    if [[ $IS_PRIMARY == *"primary"* ]]; then
#      TRAY_POSITION=right
#    else
#      TRAY_POSITION=none
#    fi
#    for bar in "${positional[@]}"; do
#      printf "Launching bar: ${bar}\n"
#      TRAY_POSITION="${TRAY_POSITION}" MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload "${bar}" &
#    done
#  done
#else
#  polybar-msg cmd restart
#fi
#
