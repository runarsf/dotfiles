# ============================================== #
#
# PERSONAL ~/.zshrc FILE for ARCH
#+ by Runar Fredagsvik [runarsf@protonmail.com]
#
# ============================================== #

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

export WINEPREFIX="$HOME/.wine_osu" # This is the path to a hidden folder in your home folder.
export WINEARCH=win32 # Only needed when executing the first command with that WINEPREFIX

# Arch Linux/wine-osu users should uncomment next line
# to update PATH to make sure we're using the right Wine binary
export PATH=/opt/wine-osu/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/rufus/.oh-my-zsh

# Path
PATH=$PATH:$HOME/bin/

# ZSH_THEME="powerlevel9k/powerlevel9k"
ZSH_THEME="robbyrussell"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd.mm.yyyy"
export VISUAL=vim
export EDITOR="$VISUAL"

plugins=(
  git
  zsh-autosuggestions
  zsh-history-substring-search
  zsh-syntax-highlighting
)

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'

source $ZSH/oh-my-zsh.sh
alias zshconfig="mate ~/.zshrc"
alias ohmyzsh="mate ~/.oh-my-zsh"

# Personal Aliases
alias please='sudo $(fc -ln -1)'
alias ..='cd ..'
alias cls='clear'
alias zshrc='vim ~/.zshrc'
alias reload='. ~/.zshrc'
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias ls='ls -la --color'
alias lx='ls -lXB'         #  Sort by extension.
alias lk='ls -lSr'         #  Sort by size, biggest last.
alias lt='ls -ltr'         #  Sort by date, most recent last.
alias lc='ls -ltcr'        #  Sort by/show change time,most recent last.
alias lu='ls -ltur'        #  Sort by/show access time,most recent last.
alias rr='ranger'
alias dot='cd ~/dotfiles/'
alias killpoly='killall -q polybar'
alias i3cfg='vim ~/.config/i3/config'
alias gi3cfg='gedit ~/.config/i3/config'
alias polycfg='gedit ~/.config/polybar/config'
alias firefoxcss='cd ~/.mozilla/firefox/uv4gtk2c.default/chrome/'

/usr/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

# chmod u=rwx,g=rwx,o=rwx ~/.zshrc

clear

