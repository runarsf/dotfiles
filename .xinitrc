#!/usr/bin/env sh

# Runs when you start X from outside of a Display Manager. Used to set up the X session.

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

xset +fp "${HOME:-~}/.local/share/fonts/"
xset +fp "${HOME:-~}/.fonts/"
xset fp rehash
# fc-cache -f &

# Increase key repeat rate
xset r rate 300 50 &

# Turn off system beep
xset b off &
xset b 0 0 0 &

# Turn caps into escape
setxkbmap -option caps:escape -v no &

# Make sure cursor is not an X when hovering over root window
xsetroot -cursor_name left_ptr &

if test command -v "numlockx" &>/dev/null; then
  numlockx on
fi

lxpolkit &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/polkit-kde-authentication-agent-1 &

# Start common apps
autorandr --change &
dunst --config "${XDG_CONFIG_HOME:-${HOME:-~}/.config}/dunst/dunstrc" &
# xscreensaver -no-splash &
# xfce4-power-manager &
(nitrogen --restore || ("${HOME:-~}/.fehbg" || feh --bg-scale "${XDG_CONFIG_HOME:-${HOME:-~}/.config}/wall.jpg")) &
easyeffects --gapplication-service &
if test -x /usr/bin/nm-applet; then
  nm-applet --sm-disable &
fi

# picom --config &

xdg-mime default "${BROWSER}.desktop" x-scheme-handler/http
xdg-mime default "${BROWSER}.desktop" x-scheme-handler/https

exec xmonad
