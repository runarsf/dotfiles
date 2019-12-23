#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
#while pgrep -x polybar >/dev/null; do sleep 1; done
#while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Loop through all connected monitors, launch bar with tray on $mainmon and normal on others
if type "xrandr"; then
  #for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
  for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar --reload tray &
  done
fi
