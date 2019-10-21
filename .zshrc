clear

case $- in
  *i*) ;;
  *) return;;
esac

# p10k instant prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

command -v antibody > /dev/null 2>&1 \
  || (echo "Installing Antibody."; curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin) \
  && source <(antibody init)
antibody bundle <<-EOBUNDLES
	robbyrussell/oh-my-zsh path:plugins/git
	robbyrussell/oh-my-zsh path:plugins/git-extras
	robbyrussell/oh-my-zsh path:plugins/colorize
	robbyrussell/oh-my-zsh path:plugins/colored-man-pages
	robbyrussell/oh-my-zsh path:plugins/command-not-found
	robbyrussell/oh-my-zsh path:plugins/jump
	robbyrussell/oh-my-zsh path:plugins/lol
	robbyrussell/oh-my-zsh path:plugins/emoji
	robbyrussell/oh-my-zsh path:plugins/thefuck
	robbyrussell/oh-my-zsh path:plugins/common-aliases
	robbyrussell/oh-my-zsh path:plugins/docker
	robbyrussell/oh-my-zsh path:plugins/systemd
	robbyrussell/oh-my-zsh path:plugins/tmux
	zsh-users/zsh-autosuggestions
	zsh-users/zsh-history-substring-search
	zsh-users/zsh-completions
	# zsh-users/zsh-syntax-highlighting
	# djui/alias-tips
	# desyncr/auto-ls
	chrissicool/zsh-256color
	zdharma/fast-syntax-highlighting
	mollifier/cd-gitroot
	romkatv/powerlevel10k
EOBUNDLES

alias ls='ls -lAF --color'
alias grep='grep --color'
alias c='xclip -selection clipboard'
alias paste='nc termbin.com 9999'
alias please='sudo $(fc -ln -1)'
alias reload='source $HOME/.zshrc'
eval "$(thefuck --alias heck)"

rmln() {
  [ -L "$1" ] \
    && cp --remove-destination "$(readlink "$1")" "$1" \
    || echo "$1: Not a symlink."
}

goto() {
  [ -L "$1" ] \
    && cd $(dirname $(readlink -f "$1")) \
    || echo "$1: Not a symlink."
}

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

case "$(hostname)" in
  runfre-t480s)
    alias fix-monitor='xrandr --output DP-1-2 --mode 2560x1440 --output DP-1-1 --mode 2560x1440 --right-of DP-1-2 --output eDP-1 --mode 1920x1080 --right-of DP-1-1'
    export PATH="$PATH:/home/runar/git/flutter/bin"
    ;;
esac

[ -f $HOME/.zsh_aliases ] && source $HOME/.zsh_aliases
[ -f $HOME/.profile ] && source $HOME/.profile
PROMPT_EOL_MARK=''
# Fix history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
HIST_STAMPS="dd.mm.yyyy"
# SHARE_HISTORY INC_APPEND_HISTORY_TIME appendhistory
setopt INC_APPEND_HISTORY_TIME

bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
[ -f ~/.p10k.zsh ] && source "$HOME/.p10k.zsh"