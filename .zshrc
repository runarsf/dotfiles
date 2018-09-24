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

export WINEPREFIX="$HOME/.wine_osu"
export WINEARCH=win32
export PATH=/opt/wine-osu/bin:$PATH

# Path
PATH=$PATH:$HOME/bin/

ZSH_THEME_RANDOM_CANDIDATES=( "nicoulaj" "miloshadzic" "af-magic" "agnoster" "refined" "emotty" "wedisagree" "imajes" "mh" "pure"  )
ZSH_THEME="random"
#ZSH_THEME="af-magic"
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

export ZSH=$HOME/.oh-my-zsh
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
alias polycfg='gedit ~/.config/polybar/config'
alias rtheme='print $RANDOM_THEME'

# chmod u=rwx,g=rwx,o=rwx ~/.zshrc

clear

