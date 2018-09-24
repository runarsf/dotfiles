#!/bin/sh

#ICON=$HOME/.config/i4lock.png
TMPBG=/tmp/screen.png
scrot /tmp/screen.png
convert $TMPBG -scale 10% -scale 1000% $TMPBG
convert $TMPBG $ICON -gravity center -composite -matte $TMPBG
# convert $TMPBG -blur 0x9 $TMPBG # blur
i3lock -i $TMPBG
