# Profile file. Runs on login.

# Adds `~/bin` to $PATH
export PATH="${PATH}:$(du "${HOME}/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:${HOME}/go/bin"
export PATH="${PATH}:${HOME}/.gem/ruby/*/bin"
export PATH="${PATH}:/snap/bin"
export LANG="en_US.UTF-8"
export LC_ALL="${LANG}"

export EDITOR='nvim'
export VISUAL="${EDITOR}"
export FILE="${EDITOR}"
export TERMINAL="alacritty"
export BROWSER="firefox"
export PAGER="less"
export READER="zathura"
export PYTHONDONTWRITEBYTECODE=1
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Auto start ssh-agent
eval "$(ssh-agent)" && ssh-add

# Start graphical server on tty1 if not already running.
test "$(tty)" = "/dev/tty1" && ! pgrep -x Xorg >/dev/null && startx
