# runarsf's Zoomer SHell config {{{
# vim: set foldmethod=marker foldlevel=0 nomodeline:
[[ $- != *i* ]] && return # don't do anything if not running interactively
# Auto start tmux, chsh alternative: sudo chsh -s /bin/tmux ${USER}
#if command -v "tmux" >/dev/null 2>&1 && test -n "${PS1}" -a -z "${TMUX}" && [[ ! "${TERM}" =~ screen ]] && [[ ! "${TERM}" =~ tmux ]]; then
#  exec tmux
#fi
# }}}

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc. {{{
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
#if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
#fi
# }}}

# Completions {{{
#zstyle :compinstall filename "${HOME}/.config/zsh/.zshrc"
#zstyle ':completion:*:(cd|mv|cp):*' menu select ignore-parents parent pwd
autoload -Uz compinit

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

compinit
_comp_options+=(globdots)
# }}}

# plugins {{{
if test -z ${NOPLUG}; then
  command -v "antibody" >/dev/null 2>&1 \
    || (echo "Installing Antibody."; curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin) \
    && source <(antibody init)
	antibody bundle <<-EOBUNDLES
		robbyrussell/oh-my-zsh path:lib
		robbyrussell/oh-my-zsh path:plugins/colored-man-pages
		robbyrussell/oh-my-zsh path:plugins/command-not-found
		# robbyrussell/oh-my-zsh path:plugins/common-aliases # Removed because has annoying '_ > sudo' alias
		robbyrussell/oh-my-zsh path:plugins/docker
		robbyrussell/oh-my-zsh path:plugins/docker-compose
		zsh-users/zsh-autosuggestions
		zsh-users/zsh-history-substring-search
		zsh-users/zsh-completions
		djui/alias-tips
		zsh-users/zsh-syntax-highlighting
		# zdharma-continuum/fast-syntax-highlighting
		# zdharma-continuum/zsh-diff-so-fancy
		akarzim/zsh-docker-aliases
		knu/zsh-manydots-magic
		skywind3000/z.lua
		kazhala/dotbare
	EOBUNDLES
  command -v starship >/dev/null 2>&1 || (curl -fsSL https://starship.rs/install.sh | bash)
  bindkey "$terminfo[kcuu1]" history-substring-search-up
  bindkey "$terminfo[kcud1]" history-substring-search-down
else
  export ZSH="${HOME}/.oh-my-zsh"
  #ZSH_THEME='miloshadzic'
  source "${ZSH}/oh-my-zsh.sh"
  eval "$(thefuck --alias fuck)"
fi
# }}}

# Options {{{
# http://zsh.sourceforge.net/Doc/Release/Options.html
setopt MENU_COMPLETE
setopt GLOB_DOTS
CASE_SENSITIVE='false'
HYPHEN_INSENSITIVE='true'
DISABLE_AUTO_UPDATE='true'
DISABLE_UPDATE_PROMPT='true'
DISABLE_MAGIC_FUNCTIONS='false'
DISABLE_LS_COLORS='false'
DISABLE_AUTO_TITLE='false'
ENABLE_CORRECTION='true'
COMPLETION_WAITING_DOTS='true'''
PROMPT_EOL_MARK=''
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=067,underline'
FZF_COMPLETION_TRIGGER=','
#ZSH_AUTOSUGGEST_STRATEGY=(history completion) # (completion match_prev_cmd)

# History {{{
# Make sure to unset any option that writes history after session end.
alias p="test -z \"\${HISTFILE}\" && {export HISTFILE=${HISTFILE}; printf \"o.o\\n\"} || {sed -i '\$ d' ${HISTFILE}; export HISTFILE=''; printf \"-.-\\n\"}"
# The following 3 options are mutually exclusive, see `man zshoptions` for explanation.
#unsetopt INC_APPEND_HISTORY
#unsetopt APPEND_HISTORY
#unsetopt INC_APPEND_HISTORY_TIME
# This  option  both  imports  new commands from the history file,
# and also causes your typed commands to be appended to the history file
# (the latter is like specifying INC_APPEND_HISTORY,
#  which should be turned off if this option is in effect).
setopt SHARE_HISTORY
#setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
HIST_STAMPS='dd/mm/yyyy'
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=5000
HISTCONTROL=ignoreboth:erasedumps
# }}}

# }}}

# https://github.com/denysdovhan/spaceship-prompt/blob/master/docs/Options.md {{{
#SPACESHIP_PROMPT_ADD_NEWLINE=false
#SPACESHIP_USER_SHOW=false
#SPACESHIP_HOST_SHOW=false
#SPACESHIP_PROMPT_SEPARATE_LINE=false
#SPACESHIP_CHAR_SYMBOL=»
#SPACESHIP_GIT_PREFIX=""
#SPACESHIP_CHAR_SUFFIX=" "
#SPACESHIP_HG_SHOW=false
#SPACESHIP_PACKAGE_SHOW=false
#SPACESHIP_NODE_SHOW=false
#SPACESHIP_RUBY_SHOW=false
#SPACESHIP_ELM_SHOW=false
#SPACESHIP_ELIXIR_SHOW=false
#SPACESHIP_XCODE_SHOW_LOCAL=false
#SPACESHIP_SWIFT_SHOW_LOCAL=false
#SPACESHIP_GOLANG_SHOW=false
#SPACESHIP_PHP_SHOW=false
#SPACESHIP_RUST_SHOW=false
#SPACESHIP_JULIA_SHOW=false
#SPACESHIP_DOCKER_SHOW=false
#SPACESHIP_DOCKER_CONTEXT_SHOW=false
#SPACESHIP_AWS_SHOW=false
#SPACESHIP_CONDA_SHOW=false
#SPACESHIP_VENV_SHOW=false
#SPACESHIP_PYENV_SHOW=false
#SPACESHIP_DOTNET_SHOW=false
#SPACESHIP_EMBER_SHOW=false
#SPACESHIP_KUBECONTEXT_SHOW=false
#SPACESHIP_TERRAFORM_SHOW=false
#SPACESHIP_TERRAFORM_SHOW=false
#SPACESHIP_VI_MODE_SHOW=false
#SPACESHIP_JOBS_SHOW=false
#SPACESHIP_EXEC_TIME_SHOW=false
#SPACESHIP_BATTERY_SHOW=false
# }}}

# Binds {{{
#set -o vi.
#bindkey 'jk' vi-cmd-mode
bindkey '^ ' autosuggest-accept
bindkey "$terminfo[kcuu1]" up-line-or-beginning-search
bindkey "$terminfo[kcud1]" down-line-or-beginning-search
bindkey "$terminfo[cuu1]" up-line-or-beginning-search
bindkey "$terminfo[cud1]" down-line-or-beginning-search
bindkey '^[^[[D' backward-word
bindkey '^[b' backward-word
bindkey '^[^[[C' forward-word
bindkey '^[f' forward-word
bindkey "${terminfo[khome]}" beginning-of-line
bindkey '^[[H' beginning-of-line
bindkey "${terminfo[kend]}" end-of-line
bindkey '^[[F' end-of-line
bindkey '^e' edit-command-line
# }}}

# Aliases {{{
alias t='clear; todo.sh -a -c'
alias vim="${EDITOR}"
alias ls='ls -lAFh --color'
#alias ls='exa -GlxFa --colour=always'
alias grep='grep --color'
alias c='xclip -selection clipboard'
alias paste='sharenix -n -c -m f'
alias gitted='git ls-files --error-unmatch'
alias please='sudo $(fc -ln -1)'
alias dkcol='docker-compose logs -f 2>&1 | ccze -m ansi'
alias vw='vim -c VimwikiIndex'
alias wk='vim -c NV'
alias yay='paru'
# alias docker-compose='docker compose'
#please () { test -z "${1}" && eval "sudo !!" || sudo "${@}" }
dkhl () { docker inspect --format "{{json .State.Health }}" "${1}" | jq }
wim () { ${EDITOR} "$(which ${1})" "${@:2}" }
lfcd () {
  # https://github.com/slavistan/lf-gadgets/tree/master/lf-shellcd
	LF_SHELLCD_TEMPDIR="$(mktemp -d -t lf-shellcd-XXXXXX)"
	export LF_SHELLCD_TEMPDIR
	~/.config/lf/lfrun -last-dir-path "$LF_SHELLCD_TEMPDIR/lastdir" "$@"
	if [ -e "$LF_SHELLCD_TEMPDIR/changecwd" ] && \
		dir="$(cat "$LF_SHELLCD_TEMPDIR/lastdir")" 2>/dev/null; then
		cd "$dir"
	fi
	rm -rf "$LF_SHELLCD_TEMPDIR"
	unset LF_SHELLCD_TEMPDIR
}
alias lf=lfcd

#insert_sudo () { zle beginning-of-line; zle -U "sudo " }
#zle -N insert-sudo insert_sudo
#bindkey "^[s" insert-sudo
#alias tmux='tmux -f "${XDG_CONFIG_HOME:-${HOME}/.config}/tmux/tmux.conf"'
#alias paste='nc termbin.com 9999'
# }}}

# Dotfiles {{{
export DOTBARE_DIR="${HOME}/.config/dotfiles/.git"
export DOTBARE_TREE="${HOME}"

alias dotted='dotfiles ls-files --error-unmatch'
alias dirtydots='dirtygit --git-dir "${DOTBARE_DIR}" --work-tree "${DOTBARE_TREE}" --git-add "-u"'
alias dots='dotfiles'
dotfiles () {
  if test "${#}" -eq "0"; then
    set - status
  fi
  git --git-dir="${DOTBARE_DIR}" --work-tree="${DOTBARE_TREE}" "${@}"
}
# }}}

# Docker outline {{{
export DOCKER_TREE="${HOME:-~}/data/docker"
export DOCKER_BARE="${DOCKER_TREE:-${HOME:-~}/data/docker}/bare/.git"
alias docked='doutline ls-files --error-unmatch'
alias dirtydocker='dirtygit --git-dir "${DOCKER_BARE}" --work-tree "${DOCKER_TREE}" --git-add "-u"'
doutline () {
  if test "${#}" -eq "0"; then
    set -- status
  fi
  git --git-dir="${DOCKER_BARE}" --work-tree="${DOCKER_TREE}" "${@}"
}
# }}}

men () { # {{{
  # https://serverfault.com/a/206830
  local pages string
  if test -n "${2}"; then
    pages=(${@:2})
    string="${1}"
  else
    pages=${1}
  fi
  # GNU man
  man ${2:+--pager="less -p \"$string\" -G"} ${pages[@]}
  # BSD man
  # man ${2:+-P "less -p \"$string\" -G"} ${pages[@]}
}
# }}}

#magic-enter () { # {{{ (breaks zsh-autosuggestions)
#  MAGIC_ENTER_GIT_COMMAND="git status -u ."
#  MAGIC_ENTER_OTHER_COMMAND="ls -lh ."
#  if test -z "${BUFFER}"; then
#    printf "\n"
#    if git rev-parse --is-inside-work-tree &>/dev/null; then
#      if git diff-index --quiet HEAD --; then
#        eval "${MAGIC_ENTER_OTHER_COMMAND}"
#      else
#        eval "${MAGIC_ENTER_GIT_COMMAND}"
#      fi
#    else
#      eval "${MAGIC_ENTER_OTHER_COMMAND}"
#    fi
#    zle redisplay
#  else
#    zle accept-line
#  fi
#}
#zle -N magic-enter
#bindkey "^M" magic-enter
# }}}

# https://unix.stackexchange.com/a/180305
# check_ssh () { # {{{
#   [[ $3 =~ '\bssh\b' ]] || return
#   [[ -n "$SSH_AGENT_PID" && -e "/proc/$SSH_AGENT_PID" ]] \
#     && ssh-add -l >/dev/null && return
#   eval `keychain --eval id_dsa --timeout 60`
# }
# autoload -U add-zsh-hook
# add-zsh-hook preexec check_ssh
# }}}

# Add aliases to all files in .local/bin {{{
#for script in "${HOME}/.local/"bin/*.*; do
#  # Without file extension
#  if ! command -v "${$(basename -- ${script})%.*}" >/dev/null 2>&1; then
#    eval "alias ${$(basename -- ${script})%.*}=$(readlink -f -- ${script})"
#  # With file extension
#  elif ! command -v "$(basename -- ${script})" >/dev/null 2>&1; then
#    eval "alias $(basename -- ${script})=$(readlink -f -- ${script})"
#  fi
#done
# }}}

# Sourcing {{{
if test -f "${XDG_CONFIG_HOME:-${HOME:-~}/.config}/lf/lficons"; then
  LF_ICONS=$(sed "${XDG_CONFIG_HOME:-${HOME:-~}/.config}/lf/lficons" \
              -e '/^[ \t]*#/d'   \
              -e '/^[ \t]*$/d'   \
              -e 's/[ \t]\+/=/g' \
              -e 's/$/ /')
  LF_ICONS=${LF_ICONS//$'\n'/:}
  export LF_ICONS
fi

#test -f "/etc/profile.d/undistract-me.sh" && source /etc/profile.d/undistract-me.sh
test -s "${HOME}/.nvm/nvm.sh" && source "${HOME}/.nvm/nvm.sh"
#test -f "${HOME}/.config/p10k/.p10k.zsh" && source "${HOME}/.config/p10k/.p10k.zsh" || (test -f "${HOME}/.p10k.zsh" && source "${HOME}/.p10k.zsh")
export STARSHIP_CONFIG=~/.config/starship.toml
eval "$(starship init zsh)"
# }}}

# FZF is STUBBORN and will automatically add this line. Just leave it be -.-
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
