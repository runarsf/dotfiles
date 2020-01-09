#!/bin/bash

# https://www.cyberciti.biz/faq/unix-linux-bash-get-time/

range() {
  if test "${1}" -ge "${2}" -a "${1}" -le "${3}"; then
    return 0
  else
    return 1
  fi
}

verbalTime() {
  hour="${1}"
  minute="${2}"
  if test "${minute}" -eq "0"; then
    printf "${hour}"
  elif range "${minute}" "1" "15"; then
    test "${minute}" -eq "15" && minute="kvart"
    printf "${minute} over ${hour}"
  elif range "${minute}" "16" "29"; then
    minute=$((30-minute))
    echo "${minute} på halv ${hour}"
  elif test "${minute}" -eq "30"; then
    printf "halv ${hour}"
  elif range "${minute}" "31" "44"; then
    minute=$((minute-30))
    printf "${minute} over halv ${hour}"
  elif range "${minute}" "45" "59"; then
    minute=$((60-minute))
    test "${minute}" -eq "15" && minute="kvart"
    printf "${minute} på ${hour}"
  fi
}

#while true; do
#  verbalTime "11" "$((i++))"
#  sleep 0.1
#  test "${i}" -ge "60" && exit 0
#done

while true; do
  hour="$(TZ=Europe/Oslo date +'%H')"
  minute="$(TZ=Europe/Oslo date +'%M')"
  echo -ne "\r\r"
  verbalTime "${hour}" "${minute}"
  sleep 2
done

