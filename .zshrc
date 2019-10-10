clear

case $- in
    *i*) ;;
      *) return;;
esac

[ -f $HOME/.zsh_aliases ] && source $HOME/.zsh_aliases
[ -f $HOME/.profile ] && source $HOME/.profile

export TERM="xterm-256color"

ANTIGEN=$HOME/.antigen/
[ -f $ANTIGEN/antigen.zsh ] || git clone\
    https://github.com/zsh-users/antigen.git $ANTIGEN
if [[ -f $ANTIGEN/antigen.zsh ]]; then
  source $ANTIGEN/antigen.zsh
  antigen use oh-my-zsh
  antigen bundle git
  antigen bundle git-extras
  antigen bundle colorize
  antigen bundle command-not-found
  antigen bundle zsh-users/zsh-syntax-highlighting
  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle zsh-users/zsh-history-substring-search
  antigen bundle jump
  antigen bundle thefuck

  antigen theme romkatv/powerlevel10k

  #antigen theme runarsf/rufus-zsh-theme
  #antigen theme rufus-minimal

  antigen apply
fi

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'
HIST_STAMPS="dd.mm.yyyy"
PROMPT_EOL_MARK=''

alias c='xclip -selection clipboard'
alias please='sudo $(fc -ln -1)'
alias reload='source $HOME/.zshrc'
alias ls='ls -lAF --color'
alias i3cfg='vim $HOME/.config/i3/config'
alias countryroads='cd ~'
alias j='jump'
alias paste='nc termbin.com 9999'
eval "$(thefuck --alias fuck)"

dirtygit() {
  printf "\n\e[94m| ADDING \e[0;39m\n\n"
  git add .
  printf "\n\e[94m| COMMITTING \e[0;39m'$*'\n\n"
  git commit -m "$*"
  printf "\n\e[94m| PULLING \e[0;39m\n\n"
  git pull
  printf "\n\e[94m| PUSHING \e[0;39m\n\n"
  git push
  printf "\n\e[32m| DONE! \e[0;39m\n\n"
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
