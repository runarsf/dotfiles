#!/bin/bash
set -e
# Debugging
#set -x
#trap 'printf "%3d: " "$LINENO"' DEBUG

COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[1;32m'
COLOR_ORANGE='\033[0;33m'
COLOR_YELLOW='\033[1;33m'
COLOR_PURPLE='\033[1;35m'
COLOR_CYAN='\033[1;36m'
COLOR_NONE='\033[0m'

os=`cat /etc/os-release | grep ^NAME`
if ! [[ $os == *Antergos* ]] || [[ $os == *Arch* ]] || [[ $os == *Ubuntu* ]] || [[ $os == *CentOS* ]]; then
	printf "${COLOR_RED}No supported OS detected, terminating script."
	printf "\nPlease refer to ${COLOR_ORANGE}/etc/os-release${COLOR_RED}, ${COLOR_ORANGE}./deploy.sh -o ${COLOR_RED}and ${COLOR_ORANGE}./deploy.sh -h ${COLOR_RED}for more info.${COLOR_NONE}\n\n"
	exit 1
fi

function run {
	#if [ $EUID != 0 ]; then
	#	sudo "$0" "-${arg}"
	#	exit $?
	#fi

	printf "${COLOR_YELLOW}\n`date`\n\n${COLOR_NONE}"

	printf "\n${COLOR_RED}This script may ask for ${COLOR_ORANGE}sudo${COLOR_RED} rights at one point, this is to make sure all configs and packages are deployed correctly."
	printf "\nIf the current terminal has sudo rights, you will not get a sudo-prompt."
	printf "\nIf this dialog appears when running with ${COLOR_ORANGE}-h${COLOR_RED}(or any other non-deployment args), click ${COLOR_ORANGE}y${COLOR_RED}."
	printf "\n\n${COLOR_ORANGE}"
	read -p "Are you sure? This will overwrite your current config files. [y/N] " -n 1 -r
	printf "${COLOR_NONE}\n\n"
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		printf "\n"
	elif [[ $REPLY =~ ^[Nn]$ ]]; then
		printf "Exiting...\n\n"
		exit 0
	else
		printf "Exiting...\n\n"
		exit 1
	fi
}

function helpme {
	printf "\n\t ${COLOR_GREEN}"
	printf                            "\n\t    ███████╗██╗██╗     ███████╗███████╗"
	printf                            "\n\t    ██╔════╝██║██║     ██╔════╝██╔════╝"
	printf                            "\n\t    █████╗  ██║██║     █████╗  ███████╗"
	printf                            "\n\t    ██╔══╝  ██║██║     ██╔══╝  ╚════██║"
	printf "\n\t ${COLOR_CYAN}██╗${COLOR_GREEN}██║     ██║███████╗███████╗███████║"
	printf "\n\t ${COLOR_CYAN}╚═╝${COLOR_GREEN}╚═╝     ╚═╝╚══════╝╚══════╝╚══════╝"
	printf "${COLOR_NONE}"
	printf "\n\n\t ${COLOR_ORANGE}This script may install other related prerequisites/versions of the listed packages..."
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-d | --desktop]"
	printf "\n\t\t ${COLOR_PURPLE}zsh | gvim | git | rofi | urxvt | i3 | polybar | ranger | compton | python(pip) | tmux | dos2unix | irssi"
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-s | --server]"
	printf "\n\t\t ${COLOR_PURPLE}zsh | vim | git | tmux | dos2unix"
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-f | --full]"
	printf "\n\t\t ${COLOR_PURPLE}Run every setup step automatically."
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-m | --monitors]"
	printf "\n\t\t ${COLOR_PURPLE}Set up monitors for use with configs. Currently only 1-3 monitors are supported."
	printf "\n\t\t ${COLOR_PURPLE}Rearrange may be needed, edit ~/.monitors to your liking. Main monitor on line 2."
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-c | --configs]"
	printf "\n\t\t ${COLOR_PURPLE}Deploy configs."
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-h | --help]"
	printf "\n\t\t ${COLOR_PURPLE}Show this dialog."
	printf "\n\n\t ${COLOR_CYAN}./deploy.sh [-o | --os]"
	printf "\n\t\t ${COLOR_PURPLE}Show supported operating systems/distros."
	printf "\n\n ${COLOR_NONE}"
}

function os {
	printf "\n\t ${COLOR_GREEN}"
	printf "\n\t ████████╗███████╗"
	printf "\n\t ██╔═══██║██╔════╝"
	printf "\n\t ██║   ██║███████╗"
	printf "\n\t ██║   ██║╚════██║"
	printf "\n\t ████████║███████║"
	printf "\n\t  ╚══════╝╚══════╝"
	printf "${COLOR_NONE}"
	printf "\n\n\t ${COLOR_ORANGE}This is a list of supported distr${COLOR_YELLOW}os${COLOR_ORANGE}."
	printf "\n\n\t ${COLOR_ORANGE}Don't see your OS/distro? Submit an issue at the github repo!"
	printf "\n\t ${COLOR_YELLOW}https://github.com/runarsf/dotfiles"
	printf "\n\n\t ${COLOR_CYAN}- Arch"
	printf "\n\t\t ${COLOR_PURPLE}- Antergos"
	printf "\n\n\t ${COLOR_CYAN}- Ubuntu"
	printf "\n\t\t ${COLOR_PURPLE}- Windows Subsystem for Linux"
	printf "\n\n\t ${COLOR_CYAN}- CentOS"
	printf "\n\n ${COLOR_NONE}"
}

function desktop {
	check yaourt
	check git
	check zsh
	check yay
	check gvim
	check rofi
	check rxvt-unicode
	check i3-wm
	check polybar
	check ranger
	check compton
	check python
	check tmux
	check blueman
	check dos2unix
	check irssi
	oh-my-zsh
}
function server {
	check git
	check zsh
	check vim
	check tmux
	check dos2unix
	oh-my-zsh
}

function check {
	if [[ $os == *Ubuntu* ]]; then
		if dpkg -s $1 | grep Status; then
			late $1
		else
			printf "sudo apt-get install $1"
			printf "\n${COLOR_PURPLE}Installed ${COLOR_GREEN}$1${COLOR_PURPLE}.${COLOR_NONE}\n\n"
		fi
	elif [[ $os == *Antergos* ]] || [[ $os == *Arch* ]]; then
		if [[ "$1" == "polybar" ]] && ! [[ `pacman -Qm polybar` ]] > /dev/null; then
			yay -S $1
		elif pacman -Qs $1 > /dev/null; then
			late $1
		else
			sudo pacman -S $1 --noconfirm
			printf "\n${COLOR_PURPLE}Installed ${COLOR_GREEN}$1${COLOR_PURPLE}.${COLOR_NONE}\n\n"
		fi
	elif [[ $os == *CentOS* ]]; then
		if yum list installed "$1" >/dev/null 2>&1; then
			late $1
  		else
			printf "sudo yum instal $1"
			printf "\n${COLOR_PURPLE}Installed ${COLOR_GREEN}$1${COLOR_PURPLE}.${COLOR_NONE}\n\n"
		fi
	else
		exit 1
	fi
}

function late {
	printf "${COLOR_CYAN} $1 ${COLOR_PURPLE}already installed.${COLOR_NONE}\n"
}

function oh-my-zsh {
	if [ ! -d "$HOME/.oh-my-zsh/" ]; then
		check curl
		check wget
		if pacman -Qs curl > /dev/null; then
			sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
		else
			sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
		fi
	else
		late oh-my-zsh
	fi
}

function configs {
	# ln -s fromFolderOrFileFullPath toThisLocationPathToFolderOrFile
	# ln -s ~/git/fonts/ ~/.fonts
	wd=`pwd`
	now="$(date +"%a_%d_%b_%Y_%H_%M_%S")"
	if ! [[ -d ~/deployBackup ]]; then
		mkdir ~/deployBackup
	fi
	mkdir ~/deployBackup/$now
	for f in * .*; do
		if ! [[ "$f" =~ ^(\.|\.\.|README\.md|\.git|\.gitignore|deploy\.sh|games|root)$ ]]; then
			printf "${COLOR_GREEN}$f${COLOR_NONE}\n"
			if [[ "$f" == "root" ]]; then
				ln -s $wd/$f/* /
			elif [[ -d "$f" ]]; then
				if [[ -d ~/$f ]]; then
					mv ~/$f ~/deployBackup/$now/
				fi
				ln -s $wd/$f/ ~/$f
			elif [[ -f "$f" ]]; then
				if [[ -f ~/$f ]]; then
					mv ~/$f ~/deployBackup/$now/
				fi
				ln -s $wd/$f ~/$f
			fi
		fi
	done
}

function monitors {
	xmonLines=`xrandr | grep " connected" | while read line ; do echo 'i' ; done`
	regex="^(\w+)\s+.+$"
	let "int=1"
	if [[ -f ~/.monitor ]]; then
		rm ~/.monitor
	fi
	for i in $xmonLines; do
		xmon=`xrandr | grep " connected" | sed $int!d`
		if [[ $xmon =~ $regex ]]; then
    		printf "${COLOR_GREEN}${BASH_REMATCH[1]}${COLOR_NONE}\n"
			echo "${BASH_REMATCH[1]}" >> ~/.monitor
		fi
		let "int++"
	done
}

# arguments
#while getopts "e:cdmsho" arg; do
#	case ${arg} in
#		m)
#			shift $((OPTIND -1))
#			exit 0;;
#	esac
#done
# make sure the script is run correctly
if [ "$#" -gt 1 ]; then
	helpme
	printf "${COLOR_RED}Too ${COLOR_ORANGE}many ${COLOR_RED}arguments!${COLOR_NONE}\n\n"
	exit 1
fi
if [ "$#" -lt 1 ]; then
	helpme
	printf "${COLOR_RED}Too ${COLOR_ORANGE}few ${COLOR_RED}arguments!${COLOR_NONE}\n\n"
	exit 1
fi

if [[ $1 == "" ]]; then exit 1
elif [[ $1 == "-m" ]] || [[ $1 == "--monitors" ]]; then
	monitors
	exit 0
elif [[ $1 == "-c" ]] || [[ $1 == "--configs" ]]; then
	run
	configs
	exit 0
elif [[ $1 == "-d" ]] || [[ $1 == "--desktop" ]]; then
	run
	desktop
	exit 0
elif [[ $1 == "-s" ]] || [[ $1 == "--server" ]]; then
	run
	server
	exit 0
elif [[ $1 == "-f" ]] || [[ $1 == "--full" ]]; then
	run
	desktop
	monitors
	configs
	exit 0
elif [[ $1 == "-h" ]] || [[ $1 == "--help" ]]; then
	helpme
	exit 0
elif [[ $1 == "-o" ]] || [[ $1 == "--os" ]]; then
	os
	exit 0
else
	helpme
	exit 1
fi
