#!/bin/bash
set -e
#set -o verbose

printf "\n`date`\n\n"

COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[1;32m'
COLOR_ORANGE='\033[0;33m'
COLOR_YELLOW='\033[1;33m'
COLOR_PURPLE='\033[1;35m'
COLOR_CYAN='\033[1;36m'
COLOR_NONE='\033[0m'

run () {
	if [ $EUID != 0 ]; then
		sudo "$0" "$@"
		exit $?
	fi

	printf "\n${COLOR_RED}This script will ask for ${COLOR_ORANGE}sudo${COLOR_RED} rights at one point, this is to make sure all configs are deployed correctly."
	printf "\nIf the current terminal has sudo rights, you will not get a sudo-prompt."
	printf "\nIf this dialogue appears when running with the ${COLOR_ORANGE}--help${COLOR_RED}(or any other non-deployment args), click ${COLOR_ORANGE}y${COLOR_RED}.${COLOR_ORANGE}\n\n"
	read -p "Are you sure? This will override your current config files. [y/n] " -n 1 -r
	printf "\n\n"
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		printf "${COLOR_NONE}\n\n"
	elif [[ $REPLY =~ ^[Nn]$ ]]; then
		printf "${COLOR_NONE}Exiting...\n\n"
		exit 0
	else
		printf "${COLOR_NONE}Exiting...\n\n"
		exit 1
	fi
}

helpme () {
	printf "\n\t ${COLOR_GREEN}"
	printf                            "\n\t    ███████╗██╗██╗     ███████╗███████╗"
	printf                            "\n\t    ██╔════╝██║██║     ██╔════╝██╔════╝"
	printf                            "\n\t    █████╗  ██║██║     █████╗  ███████╗"
	printf                            "\n\t    ██╔══╝  ██║██║     ██╔══╝  ╚════██║"
	printf "\n\t ${COLOR_CYAN}██╗${COLOR_GREEN}██║     ██║███████╗███████╗███████║"
	printf "\n\t ${COLOR_CYAN}╚═╝${COLOR_GREEN}╚═╝     ╚═╝╚══════╝╚══════╝╚══════╝"
	printf "${COLOR_NONE}"
	printf "\n\n\t ${COLOR_ORANGE}This script may install other related prerequisites/versions of the listed packages..."
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh desktop"
	printf "\n\t\t ${COLOR_PURPLE}zsh | gvim | git | rofi | urxvt | i3 | polybar | ranger | compton | python(pip) | tmux | dos2unix"
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh server"
	printf "\n\t\t ${COLOR_PURPLE}zsh | vim | git | tmux | dos2unix"
	printf "\n\n ${COLOR_NONE}"
}

desktop() {
	check git
	check zsh
	oh-my-zsh
	check gvim
	check rofi
	check rxvt-unicode
	check i3
	check polybar
	check ranger
	check compton
	check python
	check tmux
	check dos2unix
	configs
	rm $HOME/.zshrc.server
}
server() {
	check git
	check zsh
	oh-my-zsh
	check vim
	check tmux
	check dos2unix
	configs
	mv $HOME/.zshrc.server $HOME/.zshrc
}

os=`cat /etc/os-release | grep NAME`
if [[ $os == *Ubuntu* ]]; then
	pkgmgr='apt-get install'
elif [[ $os == *Antergos* ]] || [[ $os == *Arch* ]]; then
	pkgmgr='pacman -S'
elif [[ $os == *CentOS* ]]; then
	pkgmgr='yum install'
else
	printf "${COLOR_RED}No supported OS or WSL detected. Check ${COLOR_ORANGE}/etc/os-release ${COLOR_RED}for more info.${COLOR_NONE}\n\n"
	exit 1
fi

check() {
	if [[ $os == *Ubuntu* ]]; then
		pkg=`dpkg -s $1 | grep Status`
		if [[ $pkg == *installed ]]; then
			late $1
		else
			sudo $pkgmgr $1
			printf "\n${COLOR_PURPLE} Installed ${COLOR_GREEN}$1${COLOR_PURPLE}.${COLOR_NONE}\n\n"
		fi
	elif [[ $os == *Antergos* ]] || [[ $os == *Arch* ]]; then
		pkg=`pacman -Qs $1`
		if [ $pgk == "" ]; then
			sudo $pgkmgr $1
			printf "\n${COLOR_PURPLE} Installed ${COLOR_GREEN}$1${COLOR_PURPLE}.${COLOR_NONE}\n\n"
		else
			late $1
		fi
	fi
}

late() {
	printf "\n${COLOR_CYAN} $1 ${COLOR_PURPLE}already installed.${COLOR_NONE}\n\n"
}

oh-my-zsh() {
	if [ ! -d "$HOME/.oh-my-zsh/" ]; then
		if [[ $os == *Ubuntu* ]]; then
			pkg=`dpkg -s curl | grep Status`
			if [[ $pkg == *installed ]]; then
				crwg="true"
			else
				crwg="false"
			fi
		elif [[ $os == *Antergos* ]] || [[ $os == *Arch* ]]; then
			pkg=`pacman -Qs curl`
			if [ $pkg == "" ]; then
				crwg="false"
			else
				crwg="true"
			fi
		fi
		if [$crwg == "true"]; then
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
		if [[ $f == "." ]] || [[ $f == ".." ]] || [[ $f == "README.md" ]] || [[ $ == ".git" ]] || [[ $f == ".gitignore" ]] || [[ $f == "deploy.sh" ]] || [[ $f == "games" ]]; then
			continue
		elif [[ $f == "root" ]]; then
			cp -r -p -n ./root/* /
		elif [ -d $f ]; then
			cp -r -p -n ./$f $HOME/${f}
		elif [ -f $f ]; then
			cp ./$f $HOME/
		fi
	done
}


# make sure the script is run correctly
if [ "$#" -gt 1 ]; then
	printf "\n${COLOR_RED}Too ${COLOR_ORANGE}many ${COLOR_RED}arguments!${COLOR_NONE}"
	helpme
	exit 1
fi
if [ "$#" -lt 1 ]; then
	printf "\n${COLOR_RED}Too ${COLOR_ORANGE}few ${COLOR_RED}arguments!${COLOR_NONE}"
	helpme
	exit 1
fi
# arguments
case $1 in
	-c|--config)
		run
		configs
		exit 0;;
	-d|desktop)
		run
		desktop
		exit 0;;
	-s|server)
		run
		server
		exit 0;;
	-h|--help)
		helpme
		exit 0;;
	*)
		printf "\n${COLOR_RED}Invalid argument: '$1'${COLOR_NONE}"
		helpme
		exit 0;;
esac

function exit() {
	stop=`date`
	printf "\n${stop}\n\n"
}

trap exit EXIT
