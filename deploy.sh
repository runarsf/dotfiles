#!/bin/bash
set -e
# Debugging
#set -x
#trap 'printf "%3d: " "$LINENO"' DEBUG

# terminal color variables from gtufte/dotfiles
C_RED='\033[0;31m'
C_GREEN='\033[1;32m'
C_ORANGE='\033[0;33m'
C_YELLOW='\033[1;33m'
C_PURPLE='\033[1;35m'
C_CYAN='\033[1;36m'
C_NONE='\033[0m'

os=`cat /etc/*-release | grep ^NAME`
if ! [[ $os = *"Antergos"* || $os = *"Arch"* || $os = *"Ubuntu"* || $os = *"CentOS"* ]]; then
	printf "${C_RED}No supported OS detected, terminating script."
	printf "\nPlease refer to ${C_ORANGE}/etc/*-release${C_RED}, ${C_ORANGE}./deploy.sh -o ${C_RED}and ${C_ORANGE}./deploy.sh -h ${C_RED}for more info.${C_NONE}\n\n"
	exit 1
fi

function run {
	#if [ $EUID != 0 ]; then
	#	sudo "$0" "-${arg}"
	#	exit $?
	#fi

	printf "${C_YELLOW}\n`date`\n\n${C_NONE}"

	printf "\n${C_RED}This script may ask for ${C_ORANGE}sudo${C_RED} rights at one point, this is to make sure all configs and packages are deployed correctly."
	printf "\nIf the current terminal has sudo rights, you will not get a sudo-prompt."
	printf "\nIf this dialog appears when running with ${C_ORANGE}-h${C_RED}(or any other non-deployment args), click ${C_ORANGE}y${C_RED}."
	printf "\nIMPORTANT! This script must be run in a shell that supports bash builtins, e.g. bash / zsh / ksh"
	printf "\n\n${C_ORANGE}Are you sure? This will overwrite your current config files. [y/N] " && read -n 1 -r && printf "${C_NONE}\n\n"
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
	printf "\n\t ${C_GREEN}"
	printf                            "\n\t    ███████╗██╗██╗     ███████╗███████╗"
	printf                            "\n\t    ██╔════╝██║██║     ██╔════╝██╔════╝"
	printf                            "\n\t    █████╗  ██║██║     █████╗  ███████╗"
	printf                            "\n\t    ██╔══╝  ██║██║     ██╔══╝  ╚════██║"
	printf "\n\t ${C_CYAN}██╗${C_GREEN}██║     ██║███████╗███████╗███████║"
	printf "\n\t ${C_CYAN}╚═╝${C_GREEN}╚═╝     ╚═╝╚══════╝╚══════╝╚══════╝"
	printf "${C_NONE}"
	printf "\n\n\t ${C_ORANGE}This script may install other related prerequisites/versions of the listed packages..."
	printf "\n\n\t ${C_CYAN}./deploy.sh [-d | --desktop]"
	printf "\n\t\t ${C_PURPLE}zsh | gvim | git | rofi | urxvt | i3 | polybar | ranger | compton | python(pip) | tmux | dos2unix | irssi"
	printf "\n\n\t ${C_CYAN}./deploy.sh [-s | --server]"
	printf "\n\t\t ${C_PURPLE}zsh | vim | git | tmux | dos2unix"
	printf "\n\n\t ${C_CYAN}./deploy.sh [-f | --full]"
	printf "\n\t\t ${C_PURPLE}Run every setup step automatically."
	printf "\n\n\t ${C_CYAN}./deploy.sh [-m | --monitors]"
	printf "\n\t\t ${C_PURPLE}Set up monitors for use with configs. Currently only 1-3 monitors are supported."
	printf "\n\t\t ${C_PURPLE}Rearrange may be needed, edit ~/.monitors to your liking. Main monitor on line 2."
	printf "\n\n\t ${C_CYAN}./deploy.sh [-c | --configs]"
	printf "\n\t\t ${C_PURPLE}Deploy configs."
	printf "\n\n\t ${C_CYAN}./deploy.sh [-h | --help]"
	printf "\n\t\t ${C_PURPLE}Show this dialog."
	printf "\n\n\t ${C_CYAN}./deploy.sh [-o | --os]"
	printf "\n\t\t ${C_PURPLE}Show supported operating systems/distros."
	printf "\n\n ${C_NONE}"
}

function os {
	printf "\n\t ${C_GREEN}"
	printf "\n\t ████████╗███████╗"
	printf "\n\t ██╔═══██║██╔════╝"
	printf "\n\t ██║   ██║███████╗"
	printf "\n\t ██║   ██║╚════██║"
	printf "\n\t ████████║███████║"
	printf "\n\t  ╚══════╝╚══════╝"
	printf "${C_NONE}"
	printf "\n\n\t ${C_ORANGE}This is a list of supported distr${C_YELLOW}os${C_ORANGE}."
	printf "\n\n\t ${C_ORANGE}Don't see your OS/distro? Submit an issue at the github repo!"
	printf "\n\t ${C_YELLOW}https://github.com/runarsf/dotfiles"
	printf "\n\n\t ${C_CYAN}- Arch"
	printf "\n\t\t ${C_PURPLE}- Antergos"
	printf "\n\n\t ${C_CYAN}- Ubuntu"
	printf "\n\t\t ${C_PURPLE}- Windows Subsystem for Linux"
	printf "\n\n\t ${C_CYAN}- CentOS"
	printf "\n\n ${C_NONE}"
}

function desktop {
	check yaourt
	check git
	check zsh
	check yay
	check gvim
	check rofi
	check rxvt-unicode
	check i3-gaps
	check polybar aur
	check ttf-font-awesome-4 aur
	check ranger
	check compton
	check python
	check feh
	check tmux
	check firefox
	check xrandr
	check arandr
	check rofi
	check blueman
	check dos2unix
	check irssi
	check xclip
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

#/*FIXME*/
function check {
	if [[ $os == *Ubuntu* ]] && ! [[ "$2" == "aur" ]]; then
		if dpkg -s $1 | grep Status; then
			late $1
		else
			printf "sudo apt-get install $1"
			printf "${C_PURPLE}Installed ${C_GREEN}$1${C_PURPLE}.${C_NONE}\n"
		fi
	elif [[ $os == *Antergos* ]] || [[ $os == *Arch* ]]; then
		if [[ "$2" == "aur" ]] && ! [[ `pacman -Qm $1` ]] > /dev/null; then
			yay -S $1
		elif pacman -Qs $1 > /dev/null; then
			late $1
		else
			sudo pacman -S $1 --noconfirm
			printf "${C_PURPLE}Installed ${C_GREEN}$1${C_PURPLE}.${C_NONE}\n"
		fi
	elif [[ $os == *CentOS* ]] && ! [[ "$2" == "aur" ]]; then
		if yum list installed "$1" >/dev/null 2>&1; then
			late $1
  		else
			printf "sudo yum instal $1"
			printf "${C_PURPLE}Installed ${C_GREEN}$1${C_PURPLE}.${C_NONE}\n"
		fi
	else
		exit 1
	fi
}

function late {
	printf "${C_CYAN}$1 ${C_PURPLE}already installed.${C_NONE}\n"
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
			printf "${C_GREEN}$f${C_NONE}\n"
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

function rest {
	# set up .fonts firectory
	if [ ! -d ~/.fonts ]; then
		mkdir ~/.fonts
	fi
	git clone https://github.com/runarsf/fonts.git ~/.fonts/fonts
	mv ~/.fonts/fonts/*.* ~/.fonts/
	rm -rf ~/.fonts/fonts
	# install zsh-autosuggestions plugin
	if [ ! -d ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions ]; then
		git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
	fi
	# install rufus-zsh-theme
	if [ ! -f ~/.oh-my-zsh/custom/themes/rufus.zsh-theme ]; then
		git clone https://github.com/runarsf/rufus-zsh-theme.git ~/.oh-my-zsh/custom/themes/rufus-zsh-theme
		mv ~/.oh-my-zsh/custom/themes/rufus-zsh-theme/*.zsh-theme ~/.oh-my-zsh/custom/themes/
		rm -rf ~/.oh-my-zsh/custom/themes/rufus-zsh-theme
	fi
	# install vundle plugins (defined in ~/.vimrc)
	if [ ! -d ~/.vim/bundle/Vundle.vim ]; then
		git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
		vim +PluginInstall +qall
	fi
	# fix .Xresources include
	# .Xresources doesn't support variables, which means $HOME won't work,
	# currently fixed on other way by just using ./ instead of $HOME
	# Uncomment the following line if it doesn't work;
	#sed -i "1 s/^.*$/#include \"\/home\/$USER\/.xres\/urxvt\"/" ~/.Xresources
	# firefox theme
	if [ ! -d ~/.mozilla/firefox/*.default/chrome ]; then
		git clone https://github.com/runarsf/chrome ~/.mozilla/firefox/*.default/chrome
	fi
}

function swap_lines {
	# thanks to: https://www.reddit.com/r/commandline/comments/48md67/swapping_lines_in_a_file_with_sed/d0ur8ls
	file_name=${1}
	v1=${2} # to_be_swapped_1st_line
	v2=${3} # to_be_swapped_2nd_line
	sed -i "$v1,$v2"'{'"$v2"'{ x; s/\([^\n]\+\)\(\n\)\(.*\)/\3\2\1/g; H; x };'"$v1"'{h;d};'"$v1"'!{'"$v2"'!{H;d}}}' ${file_name}
}

function monitors {
	printf "${C_RED}Update package list? [Y/n] " && read -n 1 -r && printf "${C_NONE}\n"
	if [[ $REPLY =~ ^[Yy]$ ]] || [[ ! $REPLY =~ ^[Nn]$ ]]; then
		check xrandr
		check wmctrl
	fi
	regex="(\w+\-*\w*)\s{1}connected"
	[ -e ~/.monitor ] && printf "${C_RED}Screen layout already exists, do you want to modify it? [y/N] " && read -n 1 -r && printf "${C_NONE}\n"
	if [[ $REPLY =~ ^[Yy]$ ]] || [ ! -e ~/.monitor ]; then
		[ -e ~/.monitor ] && rm ~/.monitor
		touch ~/.monitor
		let "int=1"
		printf "\n${C_RED}Current screen layout (~/.monitor):\n"
		xrandr | grep " connected" | while read line; do
			xmon=`xrandr | grep " connected" | sed $int!d`
			printf "${C_ORANGE} $int ${C_RED} $xmon ${C_NONE}"
			if [[ $xmon =~ $regex ]]; then
				printf "${C_GREEN}${BASH_REMATCH[1]}${C_NONE}\n"
				echo "${BASH_REMATCH[1]}" >> ~/.monitor
			fi
			let "int++"
		done
	fi
	printf "${C_RED}Do you want to reposition your screens? [y/N] " && read -n 1 -r && printf "${C_NONE}\n"
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		printf "\n"
		printf "${C_GREEN}Enter 'q' at any time to exit.${C_NONE}\n"
		while [ "$position" != "q" ]; do
			printf "${C_ORANGE}Select screen to reposition (1-$(cat ~/.monitor | wc -l)): ${C_RED}"
			read selected </dev/tty
			[ "$selected" = "q" ] && break
			printf "${C_ORANGE}Where do you want to put it? (1-$(cat ~/.monitor | wc -l)): ${C_RED}"
			read position </dev/tty
			[ "$position" != "q" ] && [ "$selected" != "$position" ] && [[ ! $selected -gt $(cat ~/.monitor | wc -l) ]] && [[ ! "$position" -gt $(cat ~/.monitor | wc -l) ]] && [[ ! $selected -lt 1 ]] && [[ ! $position -lt 1 ]] && swap_lines ~/.monitor $selected $position
		done
		printf "${C_RED}Final screen layout (~/.monitor):\n"
		cat ~/.monitor
		printf "${C_NONE}"
	fi
	if [[ $(wmctrl -m | grep Name) =~ .*i3$ ]]; then
		printf "${C_RED}Running i3 instance detected, do you want to apply configs? [y/N] " && read -n 1 -r && printf "${C_NONE}"
		if [[ $REPLY =~ ^[Yy]$ ]]; then
			mon1="$(sed '1!d' ~/.monitor)"
			mon2="$(sed '2!d' ~/.monitor)"
			mon3="$(sed '$!d' ~/.monitor)"
			sed "/workspace \$ws1 output/c\workspace \$ws1 output ${mon1}" ~/.config/i3/config > ~/.mtt
			sed -i "/workspace \$ws2 output/c\workspace \$ws2 output ${mon2}" ~/.mtt
			sed -i "/workspace \$ws3 output/c\workspace \$ws3 output ${mon1}" ~/.mtt
			sed -i "/workspace \$ws4 output/c\workspace \$ws4 output ${mon1}" ~/.mtt
			sed -i "/workspace \$ws5 output/c\workspace \$ws5 output ${mon1}" ~/.mtt
			sed -i "/workspace \$ws6 output/c\workspace \$ws6 output ${mon1}" ~/.mtt
			sed -i "/workspace \$ws7 output/c\workspace \$ws7 output ${mon2}" ~/.mtt
			sed -i "/workspace \$ws8 output/c\workspace \$ws8 output ${mon2}" ~/.mtt
			sed -i "/workspace \$ws9 output/c\workspace \$ws9 output ${mon3}" ~/.mtt
			sed -i "/workspace \$ws10 output/c\workspace \$ws10 output ${mon3}" ~/.mtt
		fi
	fi
}

function finished {
	printf "${C_CYAN}Deploy script finished!${C_NONE}"
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
	printf "${C_RED}Too ${C_ORANGE}many ${C_RED}arguments!${C_NONE}\n\n"
	exit 1
fi
if [ "$#" -lt 1 ]; then
	helpme
	printf "${C_RED}Too ${C_ORANGE}few ${C_RED}arguments!${C_NONE}\n\n"
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
	rest
	exit 0
elif [[ $1 == "-f" ]] || [[ $1 == "--full" ]]; then
	run
	desktop
	monitors
	configs
	rest
	finished
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
