#!/bin/bash


C_RED='\033[0;31m'
C_GREEN='\033[1;32m'
C_ORANGE='\033[0;33m'
C_YELLOW='\033[1;33m'
C_PURPLE='\033[1;35m'
C_CYAN='\033[1;36m'
C_NONE='\033[0m'

credentials () {
	username=$1
	password=$2
	[ ! -e ~/.config/sensumon/ ] && mkdir ~/.config/sensumon
	[ ! -f ~/.config/sensumon/.config ] && touch ~/.config/sensumon/.config
	[ ! "$1" ] && username=''
	[ ! "$2" ] && password=''
	[ ! "$3" ] && url=''
}

run () {
	response=$(curl -s -u $username:$password $url/events)
	echo "$response" | jq ".[0] | .client | .name" | tr -d '"'
}

usage () {
	printf "Usage:\tsensumon.sh USERNAME PASSWORD\n"
}

credentials $1 $2 $3
if [ "$#" -gt 3 ]; then
	printf "${C_RED}Too ${C_ORANGE}many ${C_RED}arguments!${C_NONE}\n"
	usage
	exit 1
elif [ "$username" = "" ] || [ "$password" = "" ]; then
	printf "${C_RED}Could not detect ${C_ORANGE}username ${C_RED}and/or ${C_ORANGE}password${C_RED}.${C_NONE}\n"
	usage
	exit 1
else
	run
fi
