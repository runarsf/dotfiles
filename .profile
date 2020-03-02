# Profile file. Runs on login.

# Adds `~/bin` to $PATH
export PATH="${PATH}:$(du "${HOME}/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:/snap/bin"
export LANG="en_US.UTF-8"
export LC_ALL="${LANG}"

test -n "${SSH_CONNECTION}" \
  && export EDITOR='nvim' \
  || export EDITOR='nvim'
export VISUAL="${EDITOR}"
export FILE="${EDITOR}"
export TERMINAL="alacritty"
export BROWSER="firefox"
export PYTHONDONTWRITEBYTECODE=1

# Auto start ssh-agent
eval "$(ssh-agent)" && ssh-add

# Start graphical server on tty1 if not already running.
test "$(tty)" = "/dev/tty1" && ! pgrep -x Xorg >/dev/null && exec startx
