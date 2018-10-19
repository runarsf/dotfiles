# ============================================== #
#
# PERSONAL ~/.zshrc FILE for ARCH
#+ by Runar Fredagsvik [runarsf@protonmail.com]
#
# ============================================== #

PYTHONDONTWRITEBYTECODE=1 python -m pytest -p no:cacheprovider

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

ZSH_THEME_RANDOM_CANDIDATES=( "nicoulaj" "miloshadzic" "af-magic" "agnoster" "refined" "wedisagree" "imajes" "mh" "pure"  )
# ZSH_THEME="random"
ZSH_THEME="rufus"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd.mm.yyyy"
PROMPT_EOL_MARK=''
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

# alias punct='cd ~/git/punct/src/ && python punct.py'

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
alias polycfg='vim ~/.config/polybar/config'
alias rtheme='print $RANDOM_THEME'
alias nagios='ssh root@nagios.kantega.lan'
alias nagiosA='ssh root@nagios.kantega.lan -A'
alias bim='vim'
alias vi='vim'
alias notes='cd ~/notes/ && clear && ls | grep -v "\." | grep -v "total " && printf "\n"'
alias ns='cd ~/notes/ && clear && ls | grep -v "\." | grep -v "total " && printf "\n"'

# chmod u=rwx,g=rwx,o=rwx ~/.zshrc

clear

