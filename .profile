# Runs when logging in. Used for setting up the environment.
# .xprofile is run when logging in through a Display Manager.

export LANG="en_US.UTF-8"
export LC_ALL="${LANG}"

export EDITOR='nvim'
export VISUAL="${EDITOR}"
export FILE="${EDITOR}"
export TERMINAL="alacritty"
export BROWSER="firefox-developer-edition"
export PAGER="less"
export READER="zathura"
export PYTHONDONTWRITEBYTECODE=1
export ZDOTDIR="${XDG_CONFIG_HOME:-${HOME}/.config}/zsh"
export FZF_DEFAULT_OPTS="--layout=reverse --height 40%"
export XBB_URL="https://i.runarsf.dev/upload"
# export XBB_TOKEN="$(pass Screenshot/XBB_TOKEN)"
export XBB_RAW="true"
export STARSHIP_CONFIG=~/.config/starship.toml
export MANPAGER="less -X"
export LESS="--ignore-case --quit-if-one-screen --quit-on-intr FRXQ"

export BYOBU_CONFIG_DIR="${XDG_CONFIG_HOME:-${HOME}/.config}/byobu"
export CALIBRE_USE_DARK_PALETTE=1
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GTK2_RC_FILES="${HOME}/.gtkrc-2.0"

# https://stackoverflow.com/a/26825428
# > npm set prefix ~/.local
# export NPM_PACKAGES="${HOME}/.npm-packages"
# export NODE_PATH="${NPM_PACKAGES}/lib/node_modules:${NODE_PATH}"
# unset MANPATH
# export MANPATH="${NPM_PACKAGES}/share/man:$(manpath)"
# export PATH="${PATH}:${NPM_PACKAGES}/bin"

# export XDG_DATA_DIRS="${XDG_DATA_DIRS}:${HOME}/.nix-profile/share"

export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:${HOME}/.nix-profile/bin"
export PATH="${PATH}:${XDG_CONFIG_HOME:-${HOME}/.config}/bspwm/scripts"
export PATH="${PATH}:${XDG_CONFIG_HOME:-${HOME}/.config}/polybar/scripts"
export PATH="${PATH}:${HOME}/go/bin"
export PATH="${PATH}:${HOME}/.gem/ruby/*/bin"
export PATH="${PATH}:/snap/bin"
export PATH="${PATH}:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
export PATH="${PATH}:${HOME}/.local/share/miniconda3/bin"

# Test if in x session
# if xhost >& /dev/null ; then echo "Display exists"
# else echo "Display invalid" ; fi

KeyboardInterrupt () {
  printf '%s\n' "oop"
  trap - SIGINT
}

trap KeyboardInterrupt SIGINT

if [[ $- == *i* ]]; then
  # Auto start ssh-agent
  eval "$(ssh-agent)" && ssh-add

  # Start graphical server on tty1 if not already running.
  test "$(tty)" = "/dev/tty1" >/dev/null 2>&1 && ! pgrep -x Xorg >/dev/null 2>&1 && startx
fi
