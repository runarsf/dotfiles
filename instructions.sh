#!/usr/bin/env bash
# Though this is against best practices in shell scripting, `read` returns a non-zero code when reaching EOF, which means we can't exit on errors.
set +o errexit

# deploy.sh runs each line of the variable INSTRUCTIONS (formatted as a heredoc, a set of instructions).
# Each line (instruction) represents a shell command.
# An instruction could be 'sudo apt install vim', or just 'vim' to run `${prefix} vim ${suffix}`.
# This file is sourced by deploy.sh **with root privileges**, so don't do anything stupid in here...

# If an instruction doesn't contain any spaces, it will be treated as a package and run `${prefix} ${instruction} ${suffix}`.
# To get around this (for one-word commands, you could do `{instruction}; :`, or add a comment to the end of the line.

# Consider IFS='' before `read`.
# If you want to use tabs for readability, each line of the heredoc has to start with a tab character, not spaces.

# This file is responsible for setting the `prefix`, `suffix`, and `INSTRUCTIONS` variables once sourced.

testing () {
  prefix='echo'
  suffix=''
	read -r -d '' INSTRUCTIONS <<- EOINSTRUCTIONS
		hi
		echo -e 'hello'
		ls
		ls #
		ls; :
		while true; do echo hi; break; done
	EOINSTRUCTIONS
}

ubuntu () {
  prefix='DEBIAN_FRONTEND=noninteractive sudo apt-get -yq install'
  suffix='--no=install-recommends'
	read -r -d '' INSTRUCTIONS <<- EOINSTRUCTIONS
		sudo apt-get update
		zsh
		git
		curl
		curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash
		tmux
		byobu
		thefuck
		vim
		neovim
		feh
		firefox
		compton
		ranger
		rofi
		dos2unix
		irssi
		xclip
		xdotool
		sxhkd
		sshfs
		dmenu
		gpick
		libxft-dev
		build-essential
		libssl-dev
	EOINSTRUCTIONS
}

arch () {
  prefix='sudo pacman -S --noconfirm'
  suffix=''
	read -r -d '' INSTRUCTIONS <<- EOINSTRUCTIONS
		sudo pacman -Syyu
		yay
		git
		curl
		zsh
		yay -S --noconfirm lf
		vim
		rofi
		rxvt-unicode
		i3-gaps
		yay -S --noconfirm polybar
		yay -S --noconfirm ttf-font-awesome-4
		ranger
		compton
		feh
		tmux
		byobu
		firefox
		xrandr
		arandr
		blueman
		dos2unix
		irssi
		xclip
		xdotool
		zathura
		xorg-xinit
		sshfs
		i3lock
		gpick
		dmenu
		yay -S --noconfirm discord
		yay -S --noconfirm lib32-libwacom
		yay -S --noconfirm wacom-utility
		yay -S --noconfirm minecraft-launcher
		yay -S --noconfirm input-wacom-dkms
	EOINSTRUCTIONS
}

case "$(cat /etc/*-release)" in
  *Ubuntu*)
    testing;;
  *Manjaro*|*Arch*)
    arch;;
esac
