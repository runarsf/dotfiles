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
		cp -r ~/.vim/ .
		cp -r ~/.urxvt/ .
		cp ~/.fonts/*.ttf ./.fonts/
		cp ~/.fonts/*.otf ./.fonts/

		cp ~/bin/deploy.sh ./bin/
		cp ~/bin/i3lock-custom.sh ./bin/
		cp ~/bin/loadtheme.sh ./bin/
		cp ~/bin/prompt.sh ./bin/

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
