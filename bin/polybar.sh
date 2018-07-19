#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
DISPLAY1="$(xrandr -q | grep $(sed '1!d' $HOME/.monitor) | cut -d ' ' -f1)"
[[ ! -z "$DISPLAY1" ]] && MONITOR="$DISPLAY1" polybar polybarbar &

DISPLAY2="$(xrandr -q | grep $(sed '2!d' $HOME/.monitor) | cut -d ' ' -f1)"
[[ ! -z $DISPLAY2 ]] && MONITOR=$DISPLAY2 polybar polybarbar &

echo "Bars launched..."
