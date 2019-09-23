clear

case $- in
    *i*) ;;
      *) return;;
esac

[ -f $HOME/.zsh_aliases ] && source $HOME/.zsh_aliases
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh
[ -f $HOME/.profile ] && source $HOME/.profile

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
  jump
)

source $ZSH/oh-my-zsh.sh

alias c='xclip -selection clipboard'
alias please='sudo $(fc -ln -1)'
alias cls='clear'
alias reload='. $HOME/.zshrc'
alias path='echo -e ${PATH//:/\\n}'
alias ls='ls -lAF --color'
alias kbdisplay='gkbd-keyboard-display -l dvorak'
alias zshrc='vim $HOME/.zshrc'
alias i3cfg='vim $HOME/.config/i3/config'
alias countryroads='cd ~'
alias j='jump'

dirtygit() {
  printf "\n\e[94m| ADDING \e[0;39m\n\n"
  #printf "\n\e[104m \e[0;39m\e[94m ADDING \e[0;39m\n\n"
  git add .
  printf "\n\e[94m| COMMITTING \e[0;39m'$*'\n\n"
  #printf "\n\e[104m \e[0;39m\e[94m COMMITTING \e[0;39m'$*'\n\n"
  git commit -m "$*"
  printf "\n\e[94m| PULLING \e[0;39m\n\n"
  #printf "\n\e[104m \e[0;39m\e[94m PULLING \e[0;39m\n\n"
  git pull
  printf "\n\e[94m| PUSHING \e[0;39m\n\n"
  #printf "\n\e[104m \e[0;39m\e[94m PUSHING \e[0;39m\n\n"
  git push
  printf "\n\e[32m| DONE! \e[0;39m\n\n"
  #printf "\n\e[42m \e[0;39m\e[32m DONE! \e[0;39m\n\n"
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
