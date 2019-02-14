#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
#while pgrep -x polybar >/dev/null; do sleep 1; done

# Loop through all connected monitors, launch bar with tray on $mainmon and normal on others
mainmon=$(sed '1!d' $HOME/.monitor)
for m in $(polybar --list-monitors | cut -d":" -f1); do
		if [ "$m" = "$mainmon" ]; then
    	MONITOR=$m polybar --reload tray &
		else
    	MONITOR=$m polybar --reload top &
		fi
done
