# runarsf's Zoomer SHell config {{{
# vim: set foldmethod=marker foldlevel=0 nomodeline:
[[ $- != *i* ]] && return # don't do anything if not running interactively
# Auto start tmux, chsh alternative: sudo chsh -s /bin/tmux ${USER}
# if command -v "tmux" >/dev/null 2>&1 && test -n "${PS1}" -a -z "${TMUX}" && [[ ! "${TERM}" =~ screen ]] && [[ ! "${TERM}" =~ tmux ]]; then
#   exec tmux
# fi
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
command -v "antibody" >/dev/null 2>&1 \
  || (printf "Installing Antibody...\n"; curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin) \
  && source <(antibody init)
antibody bundle <<-EOBUNDLES
	robbyrussell/oh-my-zsh path:lib
	robbyrussell/oh-my-zsh path:plugins/colored-man-pages
	robbyrussell/oh-my-zsh path:plugins/command-not-found
	olets/zsh-abbr
	robbyrussell/oh-my-zsh path:plugins/docker
	robbyrussell/oh-my-zsh path:plugins/docker-compose
	zsh-users/zsh-autosuggestions
	zsh-users/zsh-history-substring-search
	zsh-users/zsh-completions
	# djui/alias-tips
	# zsh-users/zsh-syntax-highlighting
	zdharma-continuum/fast-syntax-highlighting
	zdharma-continuum/zbrowse
	zdharma-continuum/zui
	akarzim/zsh-docker-aliases
	knu/zsh-manydots-magic
	skywind3000/z.lua
	kazhala/dotbare
EOBUNDLES
command -v starship >/dev/null 2>&1 || (curl -fsSL https://starship.rs/install.sh | bash)
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

# Binds {{{
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
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
alias t='clear; todo.sh -a -c -d ${XDG_CONFIG_HOME:-${HOME:-~}/.config}/.todo.actions.d/config'
alias vim='nvim'
alias ls='ls -lAFh --color=always'
alias grep='grep --color=always'
alias c='xclip -selection clipboard'
alias gitted='git ls-files --error-unmatch'
alias dkcol='docker-compose logs -f 2>&1 | ccze -m ansi'
alias yay='paru'
abbr -S -qq yay='paru'
wim () { ${EDITOR} "$(which ${1})" "${@:2}" }
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

eval "$(starship init zsh)"
test -f ~/.fzf.zsh && source ~/.fzf.zsh
