#!/bin/bash
#set -e

gllock() {
	$(exec gllock)
}

i3lock() {
	# ICON=$HOME/.config/i4lock.png
	TMPBG=/tmp/screen.png
	scrot /tmp/screen.png
	# convert $TMPBG -scale 10% -scale 1000% $TMPBG
	convert $TMPBG -scale 10% -scale 1000% $TMPBG
	convert $TMPBG -blur 0x9 $TMPBG
	convert $TMPBG $ICON -gravity center -composite -matte $TMPBG
	# convert $TMPBG -blur 0x9 $TMPBG # blur
	i3lock -i $TMPBG
}

run() {
	export MODE=$(setxkbmap -query | awk '/variant:/ {print $2}')

	setxkbmap -option caps:super -layout no -variant ,
	$1 &&

	echo $MODE
	if [[ $MODE = "dvorak" ]]; then
		setxkbmap -option caps:super -layout no -variant dvorak
	fi
}
run $1
