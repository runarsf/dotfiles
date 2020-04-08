#!/bin/sh

id="$(xdo id -n scratchpad)"
if test -z "${id}"; then
  alacritty --class scratchpad --title scratchpad
else
  action='hide'
  if test "$(xprop -id ${id} | awk '/window state: / {print $3}')" = 'Withdrawn'; then
    action='show'
  fi
  xdo "${action}" -n scratchpad
fi
