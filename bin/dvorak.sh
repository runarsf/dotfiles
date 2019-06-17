#!/bin/bash
export DISPLAY=:0

toggleDvorak() {
	mode=$(setxkbmap -query | awk '/variant:/ {print $2}')
	if [[ $mode = "dvorak" ]]; then
		setxkbmap -option caps:super -layout no -variant ,
		notify-send "Norwegian QWERTY"
	else
		setxkbmap -option caps:super -layout no -variant dvorak
		notify-send "Norwegian DVORAK"
	fi
}

dvorakStatus() {
	mode=$(setxkbmap -query | awk '/variant:/ {print $2}')
	if [[ $mode = "dvorak" ]]; then
		notify-send "Norwegian DVORAK"
	else
		notify-send "Norwegian QWERTY"
	fi
}

case $1 in
	toggle)
		toggleDvorak
		exit 0;;
	status)
		dvorakStatus
		exit 0;;
	*)
		toggleDvorak
		exit 0;;
esac
