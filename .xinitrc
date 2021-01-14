#!/usr/bin/env sh

# xinitrc runs automatically when you run startx.

# There are some small but important commands that need to be run when we start
# the graphical environment. I keep those commands in ~/.xprofile because that
# file is run automatically if someone uses a display manager (login screen)
# and so they are needed there. To prevent doubling up commands, I source them
# here with the line below.

test -f ~/.xprofile && source ~/.xprofile

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

if [systemd-detect-virt = "oracle"];
	/usr/bin/VBoxClient-all
fi

# e.g. `WM=bspwm startx`
if test ! -z "${WM}"; then
  exec "${WM}"
elif command -v bspwm >/dev/null 2>&1; then
  exec bspwm
elif command -v i3 >/dev/null 2>&1; then
  exec i3
elif command -v dwm >/dev/null 2>&1; then
  exec dwm
elif command -v xfce4-session >/dev/null 2>&1; then
  exec xfce4-session
else
  echo "No valid window manager found."
fi
