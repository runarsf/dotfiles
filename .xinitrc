#!/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

setxkbmap -option caps:super -v no
pulseaudio --start &
#pulseaudio --log-level=4 --log-target=stderr &
sxhkd &
nm-applet &
xrdb ~/.Xresources
compton -fcCG --xrender-sync-fence > /dev/null 2>&1 &
feh --bg-scale $HOME/.config/wall.png
xdg-settings set default-web-browser firefox.desktop

xsetwacom --set "Wacom Intuos S Pen stylus" Suppress 2
xsetwacom --set "Wacom Intuos S Pen stylus" Rawsample 4
#xsetwacom --set "Wacom Intuos S Pen stylus" Area 0 351 8211 5453
xsetwacom --set "Wacom Intuos S Pen stylus" Area 2000 2351 10210 7453
xsetwacom --set "Wacom Intuos S Pen stylus" Button 1 '0'
xsetwacom --set "Wacom Intuos S Pen stylus" Button 2 '0'
xsetwacom --set "Wacom Intuos S Pen stylus" Button 3 '0'
xsetwacom --set "Wacom Intuos S Pen stylus" MapToOutput HEAD-0

#exec i3
exec dwm
