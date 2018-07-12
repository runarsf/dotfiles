# ============================================== #
#
# PERSONAL ~/.zshrc FILE for FEDORA
#+ by Runar Fredagsvik [runarsf@protonmail.com]
#
# ============================================== #

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Path to your oh-my-zsh installation.
export ZSH=/home/rufus/.oh-my-zsh

# Path
PATH=$PATH:$HOME/bin/

# ZSH_THEME="powerlevel9k/powerlevel9k"
ZSH_THEME="robbyrussell"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd.mm.yyyy"

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
alias zshrc='cat ~/.zshrc'
alias zshcfg='vim ~/.zshrc'
alias reload='. ~/.zshrc'
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias ls='ls -la $* --color'
alias lx='ls -lXB'         #  Sort by extension.
alias lk='ls -lSr'         #  Sort by size, biggest last.
alias lt='ls -ltr'         #  Sort by date, most recent last.
alias lc='ls -ltcr'        #  Sort by/show change time,most recent last.
alias lu='ls -ltur'        #  Sort by/show access time,most recent last.
alias rr='ranger'

/usr/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

# chmod u=rwx,g=rwx,o=rwx ~/.zshrc

clear

