# vim: set foldmethod=marker foldlevel=0 nomodeline:
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# vim: set foldmethod=marker foldlevel=0 nomodeline:
# runarsf's Zoomer SHell config
[[ $- != *i* ]] && return # don't do anythign if not running interactively
clear

#autoload -U compinit
#zstyle ':completion:*' menu select
#zmodload zsh/complist
#compinit
zstyle ':completion:*' menu select
zstyle :compinstall filename "$HOME/.zshrc"
autoload -Uz compinit
compinit

# Include hidden files in autocomplete:
#_comp_options+=(globdots)

# plugins {{{
if test -z ${noplug+x}; then
  command -v antibody > /dev/null 2>&1 \
    || (echo "Installing Antibody."; curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin) \
    && source <(antibody init)
	antibody bundle <<-EOBUNDLES
		robbyrussell/oh-my-zsh path:lib
		# robbyrussell/oh-my-zsh path:plugins/git
		# robbyrussell/oh-my-zsh path:plugins/git-extras
		robbyrussell/oh-my-zsh path:plugins/colorize # sudo pip3 install pygments
		robbyrussell/oh-my-zsh path:plugins/colored-man-pages
		robbyrussell/oh-my-zsh path:plugins/command-not-found
		# robbyrussell/oh-my-zsh path:plugins/jump
		robbyrussell/oh-my-zsh path:plugins/thefuck
		robbyrussell/oh-my-zsh path:plugins/common-aliases
		# robbyrussell/oh-my-zsh path:plugins/docker
		# robbyrussell/oh-my-zsh path:plugins/systemd
		# robbyrussell/oh-my-zsh path:plugins/tmux
		# robbyrussell/oh-my-zsh path:plugins/bgnotify
		# robbyrussell/oh-my-zsh path:plugins/sudo
		# robbyrussell/oh-my-zsh path:plugins/ssh-agent
		# robbyrussell/oh-my-zsh path:plugins/fzf
		# robbyrussell/oh-my-zsh path:plugins/emoji
		zsh-users/zsh-autosuggestions
		zsh-users/zsh-history-substring-search
		zsh-users/zsh-completions
		# zsh-users/zsh-syntax-highlighting
		djui/alias-tips
		# walesmd/caniuse.plugin.zsh
		zdharma/fast-syntax-highlighting
		# chrissicool/zsh-256color
		akarzim/zsh-docker-aliases
		# skx/sysadmin-util
		zdharma/zsh-diff-so-fancy
		# sroze/docker-compose-zsh-plugin
		# b4b4r07/emoji-cli
		# runarsf/rufus-zsh-theme
		# robbyrussell/oh-my-zsh path:themes/daveverwer.zsh-theme
		# robbyrussell/oh-my-zsh path:themes/edvardm.zsh-theme
		robbyrussell/oh-my-zsh path:themes/miloshadzic.zsh-theme
		# denysdovhan/spaceship-prompt
		# romkatv/powerlevel10k
	EOBUNDLES
  bindkey "$terminfo[kcuu1]" history-substring-search-up
  bindkey "$terminfo[kcud1]" history-substring-search-down
else
  export ZSH="${HOME}/.oh-my-zsh"
  ZSH_THEME='rufus-nightly'
  source "${ZSH}/oh-my-zsh.sh"
  eval "$(thefuck --alias fuck)"
fi
# }}}

## setopt MENU_COMPLETE
## CASE_SENSITIVE='false'
## HYPHEN_INSENSITIVE='true'
#HISTFILE=~/.zsh_history
#HISTSIZE=10000
#SAVEHIST=10000
#HIST_STAMPS='dd.mm.yyyy'
#setopt INC_APPEND_HISTORY_TIME # SHARE_HISTORY INC_APPEND_HISTORY_TIME appendhistory

# Tab completion match beginning and middle of words
bindkey '\CI' expand-or-complete-prefix
#zstyle ':completion:*' completer _complete
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
#autoload -Uz compinit
#compinit

# https://github.com/denysdovhan/spaceship-prompt/blob/master/docs/Options.md {{{
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_USER_SHOW=false
SPACESHIP_HOST_SHOW=false
SPACESHIP_PROMPT_SEPARATE_LINE=false
#SPACESHIP_CHAR_SYMBOL=❯
SPACESHIP_GIT_PREFIX=""
SPACESHIP_CHAR_SUFFIX=" "
SPACESHIP_HG_SHOW=false
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_NODE_SHOW=false
SPACESHIP_RUBY_SHOW=false
SPACESHIP_ELM_SHOW=false
SPACESHIP_ELIXIR_SHOW=false
SPACESHIP_XCODE_SHOW_LOCAL=false
SPACESHIP_SWIFT_SHOW_LOCAL=false
SPACESHIP_GOLANG_SHOW=false
SPACESHIP_PHP_SHOW=false
SPACESHIP_RUST_SHOW=false
SPACESHIP_JULIA_SHOW=false
SPACESHIP_DOCKER_SHOW=false
SPACESHIP_DOCKER_CONTEXT_SHOW=false
SPACESHIP_AWS_SHOW=false
SPACESHIP_CONDA_SHOW=false
SPACESHIP_VENV_SHOW=false
SPACESHIP_PYENV_SHOW=false
SPACESHIP_DOTNET_SHOW=false
SPACESHIP_EMBER_SHOW=false
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_TERRAFORM_SHOW=false
SPACESHIP_TERRAFORM_SHOW=false
SPACESHIP_VI_MODE_SHOW=false
SPACESHIP_JOBS_SHOW=false
SPACESHIP_EXEC_TIME_SHOW=false
SPACESHIP_BATTERY_SHOW=false
# }}}

#set -o vi
#bindkey 'jk' vi-cmd-mode
bindkey '^ ' autosuggest-accept
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#ff00ff,bg=cyan,bold,underline"
PROMPT_EOL_MARK=''
alias vim="${EDITOR}"
alias ls='ls -lAFh --color'
alias grep='grep --color'
alias c='xclip -selection clipboard'
#alias paste='nc termbin.com 9999'
alias please='sudo $(fc -ln -1)'
alias reload="source ${HOME}/.zshrc"
#alias back='cd $OLDPWD'

# Change cursor shape for different vi modes.
#function zle-keymap-select {
#  if [[ ${KEYMAP} == vicmd ]] ||
#     [[ $1 = 'block' ]]; then
#    echo -ne '\e[1 q'

#  elif [[ ${KEYMAP} == main ]] ||
#       [[ ${KEYMAP} == viins ]] ||
#       [[ ${KEYMAP} = '' ]] ||
#       [[ $1 = 'beam' ]]; then
#    echo -ne '\e[5 q'
#  fi
#}
#zle -N zle-keymap-select

# Use beam shape cursor on startup.
#echo -ne '\e[5 q'
# Use beam shape cursor for each new prompt.
#preexec() { echo -ne '\e[5 q' ;}

# TODO: If previous command failed, <Enter> should run thefuck
magic-enter () {
  # If commands are not already set, use the defaults
  #[ -z "$MAGIC_ENTER_GIT_COMMAND" ] && MAGIC_ENTER_GIT_COMMAND="git status -u ."
  MAGIC_ENTER_GIT_COMMAND="git status -u ."
  #[ -z "$MAGIC_ENTER_OTHER_COMMAND" ] && MAGIC_ENTER_OTHER_COMMAND="ls -lh ."
  MAGIC_ENTER_OTHER_COMMAND="ls -lh ."
  if test -z $BUFFER; then
    echo ""
    #echo "Ret: ${RET}"
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

# Add aliases to all files in ./bin
for script in "$( dirname "$(readlink -f -- "${HOME}"/.zshrc)" )"/bin/*.*; do
  # Without file extension
  if ! command -v "${$(basename -- ${script})%.*}" >/dev/null 2>&1; then
    eval "alias ${$(basename -- ${script})%.*}=$(readlink -f -- ${script})"
  # With file extension
  elif ! command -v "$(basename -- ${script})" >/dev/null 2>&1; then
    eval "alias $(basename -- ${script})=$(readlink -f -- ${script})"
  fi
done

test -s "${HOME}/.nvm/nvm.sh" && source "${HOME}/.nvm/nvm.sh"
test -f "${HOME}/.fzf.zsh" && source "${HOME}/.fzf.zsh"
test -f "${HOME}/.config/p10k/.p10k.zsh" && source "${HOME}/.config/p10k/.p10k.zsh" || (test -f "${HOME}/.p10k.zsh" && source "${HOME}/.p10k.zsh")
