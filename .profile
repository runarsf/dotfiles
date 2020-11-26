# Profile file. Runs on login.

# Adds `~/bin` to $PATH
#export PATH="${PATH}:$(du "${HOME}/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
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
#export BROWSER="$((command -v firefox >/dev/null 2>&1 && echo firefox) || (command -v brave >/dev/null 2>&1 && echo brave))"
export BROWSER="firefox"
export PAGER="less"
export READER="zathura"
export PYTHONDONTWRITEBYTECODE=1
export ZDOTDIR="${XDG_CONFIG_HOME:-${HOME}/.config}/zsh"
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"

export BYOBU_CONFIG_DIR=${XDG_CONFIG_HOME:-${HOME}}/.config/byobu
export CALIBRE_USE_DARK_PALETTE=1
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GTK2_RC_FILES="${HOME}/.gtkrc-2.0"

# Auto start ssh-agent
eval "$(ssh-agent)" && ssh-add

# Start graphical server on tty1 if not already running.
test "$(tty)" = "/dev/tty1" >/dev/null 2>&1 && ! pgrep -x Xorg >/dev/null 2>&1 && startx
