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
if test -z ${noplug}; then
  command -v "antibody" >/dev/null 2>&1 \
    || (echo "Installing Antibody."; curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin) \
    && source <(antibody init)
	antibody bundle <<-EOBUNDLES
		robbyrussell/oh-my-zsh path:lib
		# robbyrussell/oh-my-zsh path:plugins/git
		# robbyrussell/oh-my-zsh path:plugins/git-extras
		# robbyrussell/oh-my-zsh path:plugins/colorize # sudo pip3 install pygments
		robbyrussell/oh-my-zsh path:plugins/colored-man-pages
		# robbyrussell/oh-my-zsh path:plugins/command-not-found
		# robbyrussell/oh-my-zsh path:plugins/jump
		# robbyrussell/oh-my-zsh path:plugins/thefuck
		# robbyrussell/oh-my-zsh path:plugins/common-aliases
		# robbyrussell/oh-my-zsh path:plugins/docker
		robbyrussell/oh-my-zsh path:plugins/docker-compose
		zsh-users/zsh-autosuggestions
		zsh-users/zsh-history-substring-search
		zsh-users/zsh-completions
		# djui/alias-tips
		# zsh-users/zsh-syntax-highlighting
		zdharma/fast-syntax-highlighting
		akarzim/zsh-docker-aliases
		zdharma/zsh-diff-so-fancy
		knu/zsh-manydots-magic
		# kazhala/dotbare
		# runarsf/rufus-zsh-theme path:rufus-nightly.zsh-theme
		# robbyrussell/oh-my-zsh path:themes/daveverwer.zsh-theme
		# robbyrussell/oh-my-zsh path:themes/miloshadzic.zsh-theme
		denysdovhan/spaceship-prompt
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

# Options {{{
# http://zsh.sourceforge.net/Doc/Release/Options.html
setopt MENU_COMPLETE
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt HIST_EXPIRE_DUPS_FIRST
CASE_SENSITIVE='false'
HYPHEN_INSENSITIVE='true'
HISTFILE=~/.zsh_history
HISTSIZE=5000
SAVEHIST=5000
PROMPT_EOL_MARK=''
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=067,underline'
#ZSH_AUTOSUGGEST_STRATEGY=(history completion) # (completion match_prev_cmd)
# }}}

# https://github.com/denysdovhan/spaceship-prompt/blob/master/docs/Options.md {{{
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_USER_SHOW=false
SPACESHIP_HOST_SHOW=false
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_CHAR_SYMBOL=»
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

# Aliases{{{
alias vim='${EDITOR}'
alias ls='ls -lAFh --color'
alias grep='grep --color'
alias c='xclip -selection clipboard'
alias please='sudo $(fc -ln -1)'
alias back='cd ${OLDPWD}'
alias reload='source "${HOME}/.config/zsh/.zshrc"'
alias lineinon='pactl load-module module-loopback latency_msec=1'
alias whim='whim --editor "${EDITOR} +startinsert" --terminal "${TERMINAL} --class scratchpad -e"'
dkhl () { docker inspect --format "{{json .State.Health }}" "${1}" | jq }
wim () { ${EDITOR} "$(which ${1})" "${@:2}" }
#alias tmux='tmux -f "${XDG_CONFIG_HOME:-${HOME}/.config}/tmux/tmux.conf"'
#alias paste='nc termbin.com 9999'
# }}}

# Dotfiles {{{
export DOT_DIR="${HOME}/.config/dotfiles/.git"
export DOT_TREE="${HOME}"
export DOTBARE_DIR="${DOT_DIR}"
export DOTBARE_TREE="${DOT_TREE}"

alias dotted='dotfiles ls-files --error-unmatch'
alias dirtydots="dirtygit --git-dir '${DOT_DIR}' --work-tree '${DOT_TREE}' --git-add '-u'"
alias dots='dotfiles'
dotfiles () {
  if test "${#}" -eq "0"; then
    dotfiles status
    return 0
  fi
  #for var in "${@}"; do
  #  if test "${var}" = "."; then
  #    printf "\'.\' argument disabled by dotfiles git wrapper."
  #    return 1
  #  fi
  #done
  git --git-dir="${DOT_DIR}" --work-tree="${DOT_TREE}" "${@}"
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
test -s "${HOME}/.nvm/nvm.sh" && source "${HOME}/.nvm/nvm.sh"
test -f "${HOME}/.fzf.zsh" && source "${HOME}/.fzf.zsh"
test -f "${HOME}/.config/p10k/.p10k.zsh" && source "${HOME}/.config/p10k/.p10k.zsh" || (test -f "${HOME}/.p10k.zsh" && source "${HOME}/.p10k.zsh")
# }}}
