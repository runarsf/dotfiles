#!/bin/sh

ID=$(pgrep "$1")

if [ -z "$ID" ]; then
    "$1" "$2"
  else
    killall $1 && notify-send "$1" "$1 killed"
fi

