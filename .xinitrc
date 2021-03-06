#!/usr/bin/env sh

# xinitrc runs automatically when you run startx.

# There are some small but important commands that need to be run when we start
# the graphical environment. I keep those commands in ~/.xprofile because that
# file is run automatically if someone uses a display manager (login screen)
# and so they are needed there. To prevent doubling up commands, I source them
# here with the line below.

#test -f ~/.xprofile && source ~/.xprofile

userresources=${XDG_CONFIG_HOME:-${HOME}/.config}/.urxvt/Xresources
usermodmap=${XDG_CONFIG_HOME:-${HOME}/.config}/.urxvt/Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if test -f "${sysresources}"; then
  xrdb -merge "${sysresources}"
fi

if test -f "${sysmodmap}"; then
  xmodmap "${sysmodmap}"
fi

if test -f "${userresources}"; then
  xrdb -merge "${userresources}"
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
xset +fp "${HOME}/.local/share/fonts/"
xset +fp "${HOME}/.fonts/"
xset fp rehash

# Increase key repeat rate
xset r rate 300 50 &

# Turn off system beep
xset b off &
xset b 0 0 0 &

# Swap caps and escape
setxkbmap -option caps:escape -v no &
#setxkbmap -option caps:swapescape -v no&

# Make sure cursor is not an X when hovering over root window
xsetroot -cursor_name left_ptr &

dunst &
nm-applet &
xfce4-power-manager &
picom --config "${XDG_CONFIG_HOME:-${HOME}/.config}/picom/picom.conf" -fcCGb --xrender-sync-fence &
pulseaudio --start &
(nitrogen --restore || feh --bg-scale "${XDG_CONFIG_HOME:-${HOME}/.config}/wall/wall.jpg") &
autorandr --change &

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

exec bspwm
