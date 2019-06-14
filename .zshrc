# ============================================== #
#
# PERSONAL ~/.zshrc FILE for ARCH
#+ by runarsf [runarsf@protonmail.com]
#
# ============================================== #
clear

# ============================== #
# If not running interactively
#+ don't do anything
# ============================== #
case $- in
    *i*) ;;
      *) return;;
esac

# ============================== #
# Include files
#+ .zsh_aliases: private zsh aliases
#+ travis.sh: added by travis gem
# ============================== #
[ -f $HOME/.zsh_aliases ]        && source $HOME/.zsh_aliases
[ -f $HOME/.travis/travis.sh ]   && source $HOME/.travis/travis.sh
[ -f $HOME/.profile ] 			 && source $HOME/.profile

# ============================== #
# Path and Variables
# ============================== #
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME_RANDOM_CANDIDATES=( "nicoulaj" "miloshadzic" "af-magic" "agnoster" "refined" "wedisagree" "imajes" "mh" "pure"  )
ZSH_THEME="rufus-minimal"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'

HIST_STAMPS="dd.mm.yyyy"
PROMPT_EOL_MARK=''
#COMPLETION_WAITING_DOTS="true"
#PYTHONDONTWRITEBYTECODE=1 python -m pytest -p no:cacheprovider # disable pycache

plugins=(
  git
  zsh-autosuggestions
  zsh-history-substring-search
  zsh-syntax-highlighting
)

# variables have to be loaded before source
source $ZSH/oh-my-zsh.sh

# ============================== #
# Personal Aliases
# ============================== #
alias c='xclip -selection clipboard'
alias please='sudo $(fc -ln -1)'
alias cls='clear'
alias reload='. $HOME/.zshrc'
alias path='echo -e ${PATH//:/\\n}'
alias ls='ls -lAF --color'
alias rr='ranger'

alias zshrc='vim $HOME/.zshrc'
alias i3cfg='vim $HOME/.config/i3/config'
alias polycfg='vim $HOME/.config/polybar/config'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
