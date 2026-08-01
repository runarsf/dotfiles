#!/usr/bin/env bash

# 1. Get current column and window data
WINDOW_DATA=$(niri msg --json windows | jq -r '.[] | select(.is_focused == true)')
COLUMN_WIDTH=$(echo "$WINDOW_DATA" | jq -r '.layout.tile_size[0]')

# 2. Get screen width (based on your specific JSON structure)
SCREEN_WIDTH=$(niri msg --json outputs | jq -r 'to_entries | .[0].value.logical.width')

# 3. Safety Check
if [[ -z "$SCREEN_WIDTH" || "$SCREEN_WIDTH" == "null" ]]; then
    echo "Error: Screen width not detected."
    exit 1
fi

# 4. Calculate Percentage
PERCENTAGE=$(echo "scale=2; ($COLUMN_WIDTH / $SCREEN_WIDTH) * 100" | bc -l)
PERCENTAGE_INT=$(printf "%.0f" "$PERCENTAGE")

echo "Current Width: $PERCENTAGE_INT%"

# 5. The Logic: If in Stack (30%), use consume-or-expel-window-right
if [ "$PERCENTAGE_INT" -le 35 ]; then
    echo "Window is in Stack, moving it out..."
    # This will move the window out of the stack to the right
    niri msg action consume-or-expel-window-right
    sleep 0.2
fi

# 6. Re-organize everything into Master (70%) and Stack (30%)
# Make the focused window the Master
niri msg action move-column-to-first
sleep 0.1
niri msg action set-column-width "70%"
sleep 0.1

# Move to the second column (The Stack)
niri msg action focus-column-right
sleep 0.1

# Pull all other windows into this single stack column
for i in {1..15}; do
    niri msg action consume-window-into-column
    sleep 0.05
done

# Ensure the stack is exactly 30%
niri msg action set-column-width "30%"

# Return focus to the Master
niri msg action focus-column-left
