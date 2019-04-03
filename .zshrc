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
#+ .travkeys: travis-ci/deploy keys
#+ .zsh_aliases: private zsh aliases
#+ travis.sh: added by travis gem
# ============================== #
[ -f $HOME/.travkeys ]           && source $HOME/.travkeys
[ -f $HOME/.zsh_aliases ]        && source $HOME/.zsh_aliases
[ -f $HOME/.travis/travis.sh ]   && source $HOME/.travis/travis.sh

# ============================== #
# Path and Variables
# ============================== #
PATH=$PATH:$HOME/bin/
export PATH="`ruby -e 'puts Gem.user_dir'`/bin:$PATH"

ZSH_THEME_RANDOM_CANDIDATES=( "nicoulaj" "miloshadzic" "af-magic" "agnoster" "refined" "wedisagree" "imajes" "mh" "pure"  )
ZSH_THEME="rufus" # "random"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="dd.mm.yyyy"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'
PROMPT_EOL_MARK=''
#PYTHONDONTWRITEBYTECODE=1 python -m pytest -p no:cacheprovider
export BROWSER=/usr/bin/firefox
export VISUAL=vim
export EDITOR="$VISUAL"
export TERM=rxvt
export ZSH=$HOME/.oh-my-zsh
# wine/osu!
export WINEPREFIX="$HOME/.wine_osu"
export WINEARCH=win32
export PATH=/opt/wine-osu/bin:$PATH

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
alias c='clip'
alias clip='xclip -selection clipboard'
alias npp='flatpak run org.notepad_plus_plus.Notepad-plus-plus'
alias please='sudo $(fc -ln -1)'
alias paccheck='pacman -Qi | grep Name | grep $1 | less'
alias pserver='livereload -p 5500'
alias cls='clear'
alias zshrc='vim $HOME/.zshrc'
alias reload='. $HOME/.zshrc'
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias ls='ls -la --color'
alias rr='ranger'
alias i3cfg='vim $HOME/.config/i3/config'
alias polycfg='vim $HOME/.config/polybar/config'
alias rtheme='print $RANDOM_THEME'
alias bim='vim'
alias vi='vim'
alias ns='cd $HOME/notes/ && clear && ls | grep -v "\." | grep -v "total " && printf "\n" && ranger'

# Adding this would be like ignoring all errors, not recommended
#clear
