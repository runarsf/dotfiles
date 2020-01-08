# Profile file. Runs on login.

# Adds `~/bin` to $PATH
export PATH="${PATH}:$(du "${HOME}/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:/snap/bin"

export LANG=en_US.UTF-8

test -n "${SSH_CONNECTION}" \
  && export EDITOR=/usr/bin/nvim \
  || export EDITOR=/usr/bin/nvim
export VISUAL="${EDITOR}"
export FILE="${EDITOR}"
export TERMINAL="alacritty"
export BROWSER="firefox"
export PYTHONDONTWRITEBYTECODE=1

# Start graphical server on tty1 if not already running.
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x Xorg >/dev/null && exec startx
