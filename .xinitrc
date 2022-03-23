#!/usr/bin/env sh

# xinitrc runs automatically when you run startx.

# There are some small but important commands that need to be run when we start
# the graphical environment. I keep those commands in ~/.xprofile because that
# file is run automatically if someone uses a display manager (login screen)
# and so they are needed there. To prevent doubling up commands, I source them
# here with the line below.

#test -f ~/.xprofile && source ~/.xprofile

# DESKTOP="awesome"
DESKTOP="bspwm"
# DESKTOP="xmonad" # $XDG_CACHE_HOME:-$HOME/.cache/xmonad/xmonad-x86_64-linux # to stop recompilation on every build

userresources="${XDG_CONFIG_HOME:-${HOME:-~}/.config}/.xres/Xresources"
usermodmap="${XDG_CONFIG_HOME:-${HOME:-~}/.config}/.xres/Xmodmap"
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/etc/X11/xinit/.Xmodmap"

# Load defaults and merge user settings
if test -f "${sysresources}"; then
  xrdb -merge "${sysresources}"
fi

if test -f "${userresources}"; then
  xrdb -merge "${userresources}"
fi

if test -f "${sysmodmap}"; then
  xmodmap "${sysmodmap}"
fi

if test -f "${usermodmap}"; then
  xmodmap "${usermodmap}"
fi

if test -d /etc/X11/xinit/xinitrc.d; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    test -x "${f}" && source "${f}"
  done
  unset f
fi

# TODO: Move fonts to first fontpath
xset +fp "${HOME:-~}/.local/share/fonts/"
xset +fp "${HOME:-~}/.fonts/"
xset fp rehash

# Increase key repeat rate
xset r rate 300 50 &

# Turn off system beep
xset b off &
xset b 0 0 0 &

# Turn caps into escape
setxkbmap -option caps:escape -v no &

# Make sure cursor is not an X when hovering over root window
xsetroot -cursor_name left_ptr &

# Start common apps
autorandr --change &
dunst --config "${XDG_CONFIG_HOME:-${HOME:-~}/.config}/dunst/dunstrc" &
# xscreensaver -no-splash &
# xfce4-power-manager &
# https://forums.fedoraforum.org/showthread.php?326248-Just-upgraded-pipewire-and-want-to-reload-it-without-rebooting
# pulseaudio --start &
(~/.fehbg || (feh --bg-scale ~/.config/wall.jpg || nitrogen --restore)) &
if test -x /usr/bin/nm-applet; then
  nm-applet --sm-disable &
fi
# picom --config "${XDG_CONFIG_HOME:-${HOME:-~}/.config}/picom/picom.conf" -fcCGb --xrender-sync-fence &

if test "${DESKTOP}" = "xmonad"; then
  trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent false --tint 0x13141D --height 20 --monitor "primary" &
fi

#xsetwacom --set "Wacom Intuos S Pen stylus" Suppress 2
#xsetwacom --set "Wacom Intuos S Pen stylus" Rawsample 4
#xsetwacom --set "Wacom Intuos S Pen stylus" Area 2000 2351 10210 7453
#xsetwacom --set "Wacom Intuos S Pen stylus" Button 1 '0'
#xsetwacom --set "Wacom Intuos S Pen stylus" Button 2 '0'
#xsetwacom --set "Wacom Intuos S Pen stylus" Button 3 '0'
#xsetwacom --set "Wacom Intuos S Pen stylus" MapToOutput HEAD-0

if test systemd-detect-virt = "oracle"; then
  /usr/bin/VBoxClient-all
fi

exec "${DESKTOP}"
