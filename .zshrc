clear

case $- in
  *i*) ;;
  *) return;;
esac

# p10k instant prompt (do not change execution format)
#if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
#fi

zstyle :compinstall filename "$HOME/.zshrc"
autoload -Uz compinit
compinit

if [ -z ${noplug+x} ]; then
  command -v antibody > /dev/null 2>&1 \
    || (echo "Installing Antibody."; curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin) \
    && source <(antibody init)
	antibody bundle <<-EOBUNDLES
		robbyrussell/oh-my-zsh path:lib
		robbyrussell/oh-my-zsh path:plugins/git
		robbyrussell/oh-my-zsh path:plugins/git-extras
		robbyrussell/oh-my-zsh path:plugins/colorize
		robbyrussell/oh-my-zsh path:plugins/colored-man-pages
		robbyrussell/oh-my-zsh path:plugins/command-not-found
		robbyrussell/oh-my-zsh path:plugins/jump
		robbyrussell/oh-my-zsh path:plugins/emoji
		robbyrussell/oh-my-zsh path:plugins/thefuck
		robbyrussell/oh-my-zsh path:plugins/common-aliases
		robbyrussell/oh-my-zsh path:plugins/docker
		robbyrussell/oh-my-zsh path:plugins/systemd
		robbyrussell/oh-my-zsh path:plugins/tmux
		robbyrussell/oh-my-zsh path:plugins/bgnotify
		# robbyrussell/oh-my-zsh path:plugins/magic-enter
		robbyrussell/oh-my-zsh path:plugins/fzf
		robbyrussell/oh-my-zsh path:plugins/sudo
		# robbyrussell/oh-my-zsh path:plugins/lol
		# robbyrussell/oh-my-zsh path:plugins/per-directory-history
		# robbyrussell/oh-my-zsh path:plugins/ssh-agent
		zsh-users/zsh-autosuggestions
		zsh-users/zsh-history-substring-search
		zsh-users/zsh-completions
		# zsh-users/zsh-syntax-highlighting
		djui/alias-tips
		# desyncr/auto-ls
		# walesmd/caniuse.plugin.zsh
		# molovo/revolver
		# mollifier/cd-gitroot
		zdharma/fast-syntax-highlighting
		chrissicool/zsh-256color
		akarzim/zsh-docker-aliases
		skx/sysadmin-util
		zdharma/zsh-diff-so-fancy
		sroze/docker-compose-zsh-plugin
		b4b4r07/emoji-cli
		runarsf/rufus-zsh-theme
		# robbyrussell/oh-my-zsh path:themes/miloshadzic.zsh-theme
		# romkatv/powerlevel10k
		# dracula/zsh
	EOBUNDLES
  bindkey "$terminfo[kcuu1]" history-substring-search-up
  bindkey "$terminfo[kcud1]" history-substring-search-down
else
  export ZSH="${HOME}/.oh-my-zsh"
  ZSH_THEME='rufus-nightly'
  source $ZSH/oh-my-zsh.sh
fi

set -o vi
bindkey 'jk' vi-cmd-mode

magic-enter () {
  # If commands are not already set, use the defaults
  #[ -z "$MAGIC_ENTER_GIT_COMMAND" ] && MAGIC_ENTER_GIT_COMMAND="git status -u ."
  MAGIC_ENTER_GIT_COMMAND="git status -u ."
  #[ -z "$MAGIC_ENTER_OTHER_COMMAND" ] && MAGIC_ENTER_OTHER_COMMAND="ls -lh ."
  MAGIC_ENTER_OTHER_COMMAND="ls -lh ."
  if test -z $BUFFER; then
    echo ""
    echo "Ret: ${RET}"
    #if test "${?}" -ne "0"; then
    #  heck
    if git rev-parse --is-inside-work-tree &>/dev/null; then
      if git diff-index --quiet HEAD --; then
        eval "$MAGIC_ENTER_OTHER_COMMAND"
      else
        eval "$MAGIC_ENTER_GIT_COMMAND"
      fi
    else
      eval "$MAGIC_ENTER_OTHER_COMMAND"
    fi
    zle redisplay
  else
    zle accept-line
  fi
}
zle -N magic-enter
bindkey "^M" magic-enter

if test -n "${SSH_CONNECTION}"; then
  export EDITOR=/usr/bin/nvim
else
  export EDITOR=/usr/bin/nvim
fi
export VISUAL="${EDITOR}"

alias vim='$EDITOR'
alias ls='ls -lAF --color'
alias grep='grep --color'
alias c='xclip -selection clipboard'
alias paste='nc termbin.com 9999'
alias please='sudo $(fc -ln -1)'
alias reload='source $HOME/.zshrc'
alias zshrc='$EDITOR $HOME/.zshrc'
alias back='cd "$OLDPWD"'
eval "$(thefuck --alias heck)"
# TODO: If previous command failed, <Enter> should run thefuck

alias i3cfg='${EDITOR} ${HOME}/.config/i3/config'
alias polycfg='${EDITOR} ${HOME}/.config/polybar/config'
alias zshrc='${EDITOR} ${HOME}/.zshrc'
alias vimrc='${EDITOR} ${HOME}/.vimrc'

rmln() {
  test -L "$1" \
    && cp --remove-destination "$(readlink "$1")" "$1" \
    || echo "$1: Not a symlink."
}

goto() {
  test -L "$1" \
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

debug() {
  # https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
  D_BEAK=$(tput setaf 202) # 1
  D_EYE=$(tput setaf 15) # 7
  D_BODY=$(tput setaf 11) # 3
	cat <<-EODUCK

	  ${D_BEAK}>${D_EYE}o${D_BODY})
	  (_>

	EODUCK
}

#export FZF_BASE="$HOME/.fzf"
#if ! command -v fzf > /dev/null 2>&1; then
#  git clone --depth 1 https://github.com/junegunn/fzf.git $FZF_BASE
#  $FZF_BASE/install
#fi
#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

case "$(hostname)" in
  runfre-t480s)
    alias fix-monitor='xrandr --output eDP-1 --mode 1920x1080 --output DP-1-1 --mode 2560x1440 --right-of eDP-1 --output DP-1-2 --mode 2560x1440 --right-of DP-1-1'
    alias portable='xrandr --output DP-1-2 --off --output DP-1-1 --off'
    export PATH="$PATH:/home/runar/git/flutter/bin"
    export ANDROID_HOME=/home/runar/Android/Sdk
    export PATH="${PATH}:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools"
    export JAVA_HOME=/usr/java/jre1.8.0_231
    ;;
esac

export BROWSER=/usr/bin/firefox
MAGIC_ENTER_GIT_COMMAND='git status -u .'
MAGIC_ENTER_OTHER_COMMAND='ls -lah .'
PROMPT_EOL_MARK=''
# setopt MENU_COMPLETE
# CASE_SENSITIVE='false'
# HYPHEN_INSENSITIVE='true'
JIRA_RAPID_BOARD='true'
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
HIST_STAMPS='dd.mm.yyyy'
setopt INC_APPEND_HISTORY_TIME # SHARE_HISTORY INC_APPEND_HISTORY_TIME appendhistory

# Tab completion match beginning and middle of words
bindkey '\CI' expand-or-complete-prefix
#zstyle ':completion:*' completer _complete
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
#autoload -Uz compinit
#compinit

export LANG=en_US.UTF-8
#test -f "$HOME/.zsh_aliases" && source $HOME/.zsh_aliases
#test -s "$HOME/.nvm/nvm.sh" && source "$HOME/.nvm/nvm.sh"
#test -s "$HOME/.nvm/bash_completion" && source "$HOME/.nvm/bash_completion"
#test -f "$HOME/.p10k.zsh" && source "$HOME/.p10k.zsh"
