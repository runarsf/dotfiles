#!/bin/bash
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
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh desktop"
	printf "\n\t\t ${COLOR_PURPLE}zsh | gvim | git | rofi | urxvt | i3 | polybar | ranger | compton | python(pip) | tmux"
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh server"
	printf "\n\t\t ${COLOR_PURPLE}zsh | vim | git | tmux"
	printf "\n\n"
}

desktop() {
	check git
	check zsh
	oh-my-zsh
	check gvim
	check rofi
}
server() {
	check git
	check zsh
	oh-my-zsh
	check vim
	check tmux
}

os=`cat /etc/os-release | grep NAME`
if [[ $os == *Ubuntu* ]]; then
	pkgmgr='apt-get install'
elif [[ $os == *Antergos* ]] || [[ $os == *Arch* ]]; then
	pkgmgr='pacman -S'
fi

check() {
	pkg=`dpkg -s $1 | grep Status`
	if [[ $pkg == *installed ]]; then	
		late $1
	else
		sudo $pkgmgr $1
		printf "\n${COLOR_PURPLE} Installed ${COLOR_GREEN}$1${COLOR_NONE}\n\n"
	fi
}

late() {
	printf "\n${COLOR_CYAN} $1 ${COLOR_PURPLE}already installed.${COLOR_NONE}\n\n"
}

oh-my-zsh() {
	if [ ! -d "$HOME/.oh-my-zsh/" ]; then
		pkg=`dpkg -s curl | grep Status`
		if [[ $pkg == *installed ]]; then
				sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
			else
				sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
		fi
	else
		late oh-my-zsh
	fi
}

configs() {
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
	printf "\n${COLOR_RED}Too ${COLOR_ORANGE}many ${COLOR_RED}arguments!${COLOR_NONE}\n\n"
	exit 1
fi
if [ "$#" -lt 1 ]; then
	printf "\n${COLOR_RED}Too ${COLOR_ORANGE}few ${COLOR_RED}arguments!${COLOR_NONE}\n\n"
	exit 1
fi
# arguments
case $1 in
	-d|desktop)
		desktop
		exit;;
	-s|server)
		server
		exit;;
	-h|--help)
		helpme
		exit;;
	*)
		printf "\n${COLOR_RED}Invalid argument: '$1'${COLOR_NONE}\n\n"
		exit;;
esac
