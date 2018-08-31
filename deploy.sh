#!/bin/sh
set -e
#set -o verbose

COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[1;32m'
COLOR_ORANGE='\033[0;33m'
COLOR_YELLOW='\033[1;33m'
COLOR_PURPLE='\033[1;35m'
COLOR_CYAN='\033[1;36m'
COLOR_NONE='\033[0m'

helpme () {
	printf "\n\t ${COLOR_GREEN}"
	printf "\n\t    ███████╗██╗██╗     ███████╗███████╗"
	printf "\n\t    ██╔════╝██║██║     ██╔════╝██╔════╝"
	printf "\n\t    █████╗  ██║██║     █████╗  ███████╗"
	printf "\n\t    ██╔══╝  ██║██║     ██╔══╝  ╚════██║"
	printf "\n\t ██╗██║     ██║███████╗███████╗███████║"
	printf "\n\t ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚══════╝"
	printf "${COLOR_NONE}"
	printf "\n\n\t ${COLOR_ORANGE}This script may install other related prerequisites/versions of the listed packages"
	printf "\n\n\t ${COLOR_CYAN}./deploy desktop"
	printf "\n\t\t ${COLOR_PURPLE}zsh | gvim | git | rofi | urxvt | curl | i3 | polybar | ranger | compton | python(pip)"
	printf "\n\n\t ${COLOR_CYAN}./deploy server"
	printf "\n\t\t ${COLOR_PURPLE}zsh | vim | git"
	printf "\n\n"
}

desktop () {
	zsh
}
server () {
	zsh
}

check () {
	pkg=`pacman -Qs $1`
	if [ -n "$pkg" ]
		then exit 0
		else exit 1
	fi
}

zsh () {
	exit 1
}

# deploy the config files
configs () {
	for f in `\ls -a .`
	do
		if [[ $f == "README.md" ]] || [[ $f == "deploy.sh" ]] || [[$f == "."]] || [[$f == ".."]] || [[ $f == "games" ]]; then
			continue
		fi
		if [ -d $f ]; then
			# mkdir -p $HOME/.${f}
			echo "mkdir -p $HOME/.${f}"
		fi
	done
}

# make sure the script is run correctly
if [ "$#" -gt 1 ]; then
	printf "\n${COLOR_RED}Too many arguments!${COLOR_NONE}\n\n"
	exit 1
fi
# arguments
case $1 in
	-c|--config)
		configs
		exit 0;;
	-d|desktop)
		desktop
		exit 0;;
	-m|min|minimal)
		minimal
		exit 0;;
	--help|-h)
		helpme
		exit 0;;
	*)
		printf "\n${COLOR_RED}Invalid argument: '$1'${COLOR_NONE}\n\n"
		exit 1;;
esac

