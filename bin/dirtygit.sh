#!/usr/bin/env sh
if test ! -d ".git"; then
  printf "Not a git repository."
  exit 1
fi

cleanup () {
  tput cnorm
  printf "${reset}"
  exit 1
}
trap cleanup INT

reset='\e[0;39m'
blue='\e[94m'
green='\e[32m'
yellow='\e[33m'
red='\e[31m'

dots () {
  local d='0.35'
  local i='.'
  tput civis
  printf "\r${green}${i}${i}${i}"
  sleep "${d}"
  printf "\r${yellow}${i}${i} "
  printf "\r${i}${i}"
  sleep "${d}"
  printf "\r${red}${i} "
  printf "\r${i}"
  sleep "${d}"
  printf "\r${reset} "
  printf "\r"
  sleep "${d}"
  tput cnorm
}

printf "\n${blue}| ADDING \e[0;39m\n\n"
dots
git add .

printf "\n${blue}| COMMITTING \e[0;39m'${*}'\n\n"
dots
git commit -m "${*}"

printf "\n${blue}| PULLING \e[0;39m\n\n"
dots
git pull

printf "\n${blue}| PUSHING \e[0;39m\n\n"
dots
git push

printf "\n${green}| DONE! \e[0;39m\n\n"
