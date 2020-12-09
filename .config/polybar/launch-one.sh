#!/bin/sh

CONFIG="${HOME}/.config/polybar/config"

positional=()
while test "${#}" -gt "0"; do
  case "${1}" in
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

set -- "${positional[@]}"

if test "${#}" -le "0"; then
  printf "No bars provided, using default (main).\n"
  positional=("main")
fi

killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 1; done

if [ -z "$(pgrep -x polybar)" ]; then
  for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
    IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
    if [[ $IS_PRIMARY == *"primary"* ]]; then
      TRAY_POSITION=right
    else
      TRAY_POSITION=none
    fi
    for bar in "${positional[@]}"; do
      printf "Launching bar: ${bar}\n"
      TRAY_POSITION="${TRAY_POSITION}" MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload "${bar}" &
    done
  done
else
  polybar-msg cmd restart
fi
