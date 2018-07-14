#!/bin/sh

case $1 in
	-h|--help)
		echo "Applies color scheme to urxvt config (~/.xres/urxvt)."
		echo "This requires you to include the ~/.xres/urxvt file in your urxvt config."
		echo "The urxvt config can usually be found at ~/.Xresources or ~/.Xdefaults."
		echo ""
		echo "loadtheme [theme]"
		exit
		;;
	-r|--reload)
		xrdb -load ~/.Xresources
		echo "Reloaded .Xresources"
		exit
		;;
	-a|--append)
		if [ ! -f ./$1 ]; then
			echo "No file with the name $1 found."
			exit
		else
			cat ./$1 >> ~/.xres/urxvt
			xrdb -load ~/.Xresources
			echo "Appended the file $1 to .Xresources"
		fi
		exit
		;;
	*)
		if [ ! -f ./$1 ]; then
			echo "No theme with the name $1 found."
			exit
		else
			cat ./$1 > ~/.xres/urxvt
			xrdb -load ~/.Xresources
			echo "Color scheme applied. [ $1 ]"
		fi
		exit
		;;
esac
