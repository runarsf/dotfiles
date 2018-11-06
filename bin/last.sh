#!/bin/bash

COLOR_RED='\033[0;31m'
COLOR_GREEN='\033[1;32m'
COLOR_ORANGE='\033[0;33m'
COLOR_YELLOW='\033[1;33m'
COLOR_PURPLE='\033[1;35m'
COLOR_CYAN='\033[1;36m'
COLOR_NONE='\033[0m'

saveCredentials () {
	if [[ $LASTFM_USER == "" ]]; then
		sudo echo -e "\nLASTFM_USER=\"$LASTFM_USER\"" >> /etc/environment
	else
		sudo sed 's/.*LASTFM_USER.*/LASTFM_USER=$LASTFM_USER/' /etc/environment
	fi
	printf "${COLOR_GREEN}Credentials saved!${COLOR_NONE}"
}

getCredentials () {
	if [[ $LASTFM_USER == "" ]] && [[ $LASTFM_API_KEY == "" ]]; then
		printf "${COLOR_GREEN}"
		read -p "Username: " LASTFM_USER
		printf "${COLOR_RED}"
		read -sp "API Key: " LASTFM_API_KEY

		printf "\n\n"
		read -p "Save credentials? [y/N] " choice
		case "$choice" in
		  y|Y ) saveCredentials;;
		  n|N* ) continue;;
		esac

		printf "${COLOR_CYAN}\nWelcome, $LASTFM_USER!${COLOR_NONE}"
	fi
}
getCredentials

runner () {
	prev = ''
	while True; do
		response=`curl -s "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=$LASTFM_USER&api_key=$LASTFM_API_KEY&format=json"`
		if [ $prev != $current ]; then
			printf "new"
		fi
		sleep 1
	done
}

exit 0
