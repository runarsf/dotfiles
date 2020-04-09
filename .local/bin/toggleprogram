#!/bin/bash

id="$(pgrep ${1})"

if test -z "${id}"; then
    command "${1}" "${@:2}"
  else
    killall "${1}"
fi

