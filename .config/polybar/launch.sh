#!/bin/sh

positional=()
while test "${#}" -gt "0"; do
  case "${1}" in
    --two)
      selection="traybar"
      shift;;
    --tray|-t)
      tray="true"
      shift;;
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

if test -z "$(pgrep -x polybar)"; then

  if test "${selection}" = "traybar"; then
    CONFIG="${HOME}/.config/polybar/config"
    for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
      printf "Launching bar: ${bar}\n"
      IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
      if [[ $IS_PRIMARY == *"primary"* ]]; then
        # https://manpages.ubuntu.com/manpages/bionic/man1/trayer.1.html
        MONITOR="${MONITOR}" WIDTH="92%" polybar --config="${CONFIG}" --reload main &
        MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload tray &
      else
        MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload main &
      fi
    done
  else
    CONFIG="${HOME}/.config/polybar/config"
    for MONITOR in $(polybar --list-monitors | cut -d" " -f1 | cut -d":" -f1); do
      IS_PRIMARY="$(polybar --list-monitors | grep "${MONITOR}" | cut -d" " -f3)"
      if [[ $IS_PRIMARY == *"primary"* ]] && test -n "${tray}"; then
        TRAY_POSITION=right
      else
        TRAY_POSITION=none
      fi
      for bar in "${positional[@]}"; do
        printf "Launching bar: ${bar}\n"
        TRAY_POSITION="${TRAY_POSITION}" MONITOR="${MONITOR}" polybar --config="${CONFIG}" --reload "${bar}" &
      done
    done
  fi

else
  polybar-msg cmd restart
fi
