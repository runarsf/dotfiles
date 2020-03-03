#!/usr/bin/env sh
if test ! -d "./.git"; then
  printf "Not a git repository."
  exit 1
fi

printf "\n\e[94m| ADDING \e[0;39m\n\n"
git add .
printf "\n\e[94m| COMMITTING \e[0;39m'${*}'\n\n"
git commit -m "${*}"
printf "\n\e[94m| PULLING \e[0;39m\n\n"
git pull
printf "\n\e[94m| PUSHING \e[0;39m\n\n"
git push
printf "\n\e[32m| DONE! \e[0;39m\n\n"
