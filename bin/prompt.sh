#!/bin/sh

[ $(echo "No\nYes" | dmenu -i -p "$1") == "Yes" ] && $2
