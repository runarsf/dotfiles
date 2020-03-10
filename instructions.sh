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
  aur='yay -S --noconfirm'
  suffix=''
	read -r -d '' INSTRUCTIONS <<- EOINSTRUCTIONS
		sudo pacman -Syu
		yay
		git
		curl
		zsh
		$aur lf
		vim
		rofi
		rxvt-unicode
		bspwm
		i3-gaps
		$aur polybar
		$aur ttf-font-awesome-4
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
		curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
		nvm install --lts
		$aur discord
		$aur lib32-libwacom
		$aur wacom-utility
		$aur minecraft-launcher
		$aur input-wacom-dkms
		sudo rm /etc/fonts/conf.d/70-no-bitmaps.conf
		sudo ln -s /etc/fonts/conf.avail/70-yes-bitmaps.conf /etc/fonts/conf.d/70-yes-bitmaps.conf
	EOINSTRUCTIONS
}

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

case "$(cat /etc/*-release)" in
  *) # This is used for testing, prevents all other conditions.
    testing;;
  *Ubuntu*)
    ubuntu;;
  *Manjaro*|*Arch*)
    arch;;
esac
