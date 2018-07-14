#!/bin/sh

case $1 in
	-h|--help)
		echo ""
		exit
		;;
	-c|--confirm)
		cd ~/dotfiles/

		cp ~/.zshrc .
		cp ~/.Xresources .
		cp -r ~/.xres/ .
		cp ~/.vimrc .
		cp -r ~/.urxvt/ .
		cp ~/.fonts/*.ttf ./.fonts/
		cp ~/.fonts/*.otf ./.fonts/

		cp ~/bin/*.sh ./bin/

		cp ~/.config/i3/config ./.config/i3/
		cp ~/.config/rofi/config ./.config/rofi/
		cp ~/.config/wall.png ./.config/
		cp ~/.config/ranger/rc.conf ./.config/ranger/

		exit
		;;
	*)
		exit
		;;
esac
