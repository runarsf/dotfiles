#!/bin/bash

C_RED='\033[0;31m'
C_GREEN='\033[1;32m'
C_ORANGE='\033[0;33m'
C_YELLOW='\033[1;33m'
C_PURPLE='\033[1;35m'
C_CYAN='\033[1;36m'
RESET='\033[0m'

LASTFM_API_KEY="2169b05c976f5fb53969b4ad1fce2c9c"

require () {
  if ! command -v "${1}" >/dev/null 2>&1; then
    printf "Command ${1} not found.\n"
    exit 1
  fi
}
require jq

usage () {
	cat <<-EOMAN
		${C_GREEN}Usage:${RESET} ${C_BLUE}last.sh${RESET} [${C_RED}options${RESET}]
		██╗     ███████╗███████╗████████╗███████╗██╗   ██╗
		██║     ██╔══██║██╔════╝ ╚═██╔══╝██╔════╝██║   ██║
		██║     ███████║███████╗   ██║   ███████╗████████║
		██║     ██╔══██║╚════██║   ██║   ╚════██║██╔═══██║
		███████╗██║  ██║███████║   ██║${C_CYAN}██╗${C_GREEN}███████║██║   ██║
		╚══════╝╚═╝  ╚═╝╚══════╝   ╚═╝${C_CYAN}╚═╝${C_GREEN}╚══════╝╚═╝   ╚═╝
		${RESET}"
		Usage: last.sh [args]"
		 -r\t\tRun"
		 -h\t\tHelp"
		 -c\t\tCredentials setup."
		 -d\t\tDelete credentials."
		 -p\t\tPolybar mode, returns output once."
		 -u\t\tDefine a custom user that is not saved as credentials."
	${RESET}"
	EOMAN
}

animate () {
  chars="/-\|"

  for (( i=0; i<${#chars}; i++ )); do
    sleep 0.1
    echo -en "${chars:$i:1}" "\r"
  done
}

fetch () {
  url="http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=${LASTFM_USER}&api_key=${LASTFM_API_KEY}&format=json"
  response="$(curl -s ${url})"

  if printf '%s\n' "${response}" | jq ".[] | .track | .[0] | .name" | tr -d '"'  > /dev/null; then
    if test -z "${displayArtist}" -a -z "${displayTrack}" -a -z "${displayAlbum}"; then
      displayArtist='true'; displayTrack='true'; displayAlbum='true'
    fi

    test -n "${displayArtist}" && \
    artist="$(printf '%s\n' "${response}" | jq '.[] | .track | .[0] | .artist' | tr -d '\#' | jq '.text' | tr -d '"')"

    test -n "${displayTrack}" && \
    track="$(printf '%s\n' "${response}" | jq '.[] | .track | .[0] | .name' | tr -d '"')"

    test -n "${displayAlbum}" && \
    album="$(printf '%s\n' "${response}" | jq '.[] | .track | .[0] | .album' | tr -d '\#' | jq '.text' | tr -d '"')"

    test -n "${artist}" -a -n "${track}" && separator=" - "
    if test -n "${artist}" -o -n "${track}" && test -n "${album}"; then album_separator_a=" ("; album_separator_b=")"; fi
    output="${artist}${separator}${track}${album_separator_a}${album}${album_separator_b}"

    isPlaying="$(printf '%s\n' "${response}" | jq '.[] | .track | .[0] | .["@attr"] | .nowplaying' | tr -d '"')"
    if test "${isPlaying}" != "true"; then
      #echo -en "\033[2K"
      printf "Nothing playing at the moment..."
    else
      printf "${output}"
      #echo -en "\033[2K"
      #if (( ${#curr} > `tput cols` )); then
      #  curr="--> $artist : $track"
      #echo -en "$curr\r"
    fi
  else
    echo "track lookup failed"
  fi
}

#parse () {
#
#}

#test "${#}" -lt "1" && "${0}" --help
positional=()
while test "${#}" -gt "0"; do
  case "${1}" in
    -h|--help)
      usage
      exit
      shift;;
    -a|--artist)
      displayArtist='true'
      shift;;
    -t|--track)
      displayTrack='true'
      shift;;
    -l|--album)
      displayAlbum='true'
      shift;;
    -u|--user)
      user="${2}"
      shift;shift;;
    --*)
      printf "Unknown option: ${1}\n"
      exit 1
      shift;;
    -*)
      shopts="${1}"
      if test "${#shopts}" -le "2"; then
        printf "Unknown option: ${shopts}\n"
        exit 2
      fi
      shift
      set -- "${shopts:0:2}" "-${shopts:2}" "${@}"
      ;;
    *)
      positional+=("${1}")
      shift;;
  esac
done

set -- "${positional[@]}" # restore positional parameters

if test "${#}" -gt "0"; then
  echo "Unrecognized option: ${1}"
  echo "See '${0} --help' for more info."
  exit 1
fi

#test -n "${installPackages}" && installPackages
fetch
