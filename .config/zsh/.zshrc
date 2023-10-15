# Zinit {{{
ZINIT_HOME="${XDG_DATA_HOME:-${HOME:-~}/.local/share}/zinit"

# rm -rf ${ZINIT_HOME}/plugins/* ${ZINIT_HOME}/snippets/*

# Install and load zinit {{{
if test ! -f "${ZINIT_HOME}/zinit.git/zinit.zsh"; then
  print -P '%F{33} %F{220}Installing %F{33}Zinit%F{220}...%f'
  mkdir -p "${ZINIT_HOME}" \
    && chmod g-rwX "${ZINIT_HOME}"
  git clone https://github.com/zdharma-continuum/zinit.git "${ZINIT_HOME}/zinit.git" \
    && print -P '%F{33} %F{34}Installation successful!%f%b' \
    || print -P '%F{160} Cloning failed...%f%b'
fi

source "${ZINIT_HOME}/zinit.git/zinit.zsh"

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
# }}}

# Prezto {{{
# https://github.com/sorin-ionescu/prezto/tree/master/modules
# https://github.com/sorin-ionescu/prezto/blob/master/runcoms/zpreztorc

zinit snippet PZT::modules/environment/init.zsh

zstyle ':prezto:module:terminal' auto-title 'yes'
zinit snippet PZT::modules/terminal/init.zsh

zstyle ':prezto:module:editor' dot-expansion 'yes'
zstyle ':prezto:module:editor' key-bindings 'emacs'
zstyle ':prezto:module:editor' ps-context 'yes'
zstyle ':prezto:module:prompt' managed 'yes'
zinit snippet PZTM::editor

zinit snippet PZT::modules/history/init.zsh

zinit ice wait'1' lucid
zinit snippet PZT::modules/directory/init.zsh

zinit ice wait'1' lucid
zinit snippet PZT::modules/spectrum/init.zsh

zinit snippet PZT::modules/gnu-utility/init.zsh
zstyle ':prezto:module:utility' safe-ops 'no'
zinit snippet PZTM::utility

zinit snippet PZT::modules/completion/init.zsh
# zinit snippet PZT::modules/gpg/init.zsh

# zinit ice wait'1' lucid
# zinit snippet PZT::modules/history-substring-search/init.zsh

zstyle ':prezto:*:*' case-sensitive 'no'
zstyle ':prezto:*:*' color 'yes'

zstyle ':completion:*' special-dirs false
# }}}

# Profiling - https://zdharma-continuum.github.io/zinit/wiki/Profiling-plugins/
if test ! -z "${ZPROF+x}"; then
  zinit ice atinit'zmodload zsh/zprof' \
            atload'zprof | head -n 20; zmodload -u zsh/zprof'
  zinit light zdharma-continuum/fast-syntax-highlighting
fi

zinit wait lucid for \
  atinit'ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay' \
    zdharma-continuum/fast-syntax-highlighting \
  blockf \
    zsh-users/zsh-completions \
  hlissner/zsh-autopair \
  zdharma-continuum/history-search-multi-word

zinit wait'1' lucid for \
  akarzim/zsh-docker-aliases \
  zdharma-continuum/zui \
  zdharma-continuum/zbrowse \
  skywind3000/z.lua

zinit ice wait'1' lucid
zinit snippet OMZP::colored-man-pages
# zinit snippet OMZP::per-directory-history
zinit load jimhester/per-directory-history
# _per-directory-history-set-global-history && _per_directory_history_is_global=true

zinit ice as'null' sbin'bin/*' wait'1' lucid
zinit light z-shell/zsh-diff-so-fancy

zinit ice wait'1' as'completion' lucid
zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker

zinit light nix-community/nix-zsh-completions

#zinit ice wait'1' lucid
zinit load marzocchi/zsh-notify
  zstyle ':notify:*' command-complete-timeout 15
  zstyle ':notify:*' always-check-active-window yes
  zstyle ':notify:*' check-focus no
  zstyle ':notify:*' always-notify-on-failure no
  zstyle ':notify:*' blacklist-regex 'find|git|vi|vim|nvim|neovim'
  zstyle ':notify:*' expire-time 5000

zinit light olets/zsh-abbr
  abbr -S -qq yay='paru'

#zinit ice atload'!_zsh_autosuggest_start'
# FIXME tab doesn't work
zinit light zsh-users/zsh-autosuggestions
  #bindkey '\t' autosuggest-accept
  #bindkey '^I' autosuggest-accept

# zinit ice lucid atload"unalias gcd"
# zinit snippet OMZP::git
# zinit snippet OMZP::docker
# zinit snippet OMZP::docker-compose
# zinit snippet OMZP::magic-enter

zinit light zsh-users/zsh-history-substring-search
  zmodload zsh/terminfo
  test -n "${terminfo[kcuu1]}" && bindkey "${terminfo[kcuu1]}" history-substring-search-up
  test -n "${terminfo[kcud1]}" && bindkey "${terminfo[kcud1]}" history-substring-search-down
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down

# zinit ice lucid wait from'gh-r' sbin'def-matcher'
# zinit light sei40kr/fast-alias-tips-bin
# zinit light sei40kr/zsh-fast-alias-tips

zinit ice lucid wait atclone"sed -ie 's/fc -rl 1/fc -rli 1/' shell/key-bindings.zsh" \
                atpull"%atclone" multisrc"shell/{completion,key-bindings}.zsh" id-as"junegunn/fzf_completions" \
                pick"/dev/null"
zinit light junegunn/fzf
  export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*" 2> /dev/null'
  export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"
  export FZF_ALT_C_COMMAND="fd -t d ."
  export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
  export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"

zinit ice as'command' from'gh-r' \
          atclone'./starship init zsh > init.zsh; ./starship completions zsh > _starship' \
          atpull'%atclone' src'init.zsh'
zinit light starship/starship

zinit load zshzoo/magic-enter
magic-enter-cmd () {
  if test -n "${VIRTUAL_ENV}"; then
    printf '%s' " python3 --version"
  elif git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    printf '%s' " git -c color.ui=always status --porcelain=v1 -M --show-stash --ignore-submodules"
  else
    printf '%s' " \ls --color=always -hAp"
  fi
}
# }}}

# Completions {{{
# NOTE Has to be loaded after ':prezto:module:editor'
#  see https://github.com/olets/zsh-abbr/issues/29
bindkey " " abbr-expand-and-space

# TODO prezto-equivalent
# correct_all
unsetopt correct \
         prompt_cr \
         prompt_sp

setopt globdots \
       histignorealldups \
       sharehistory \
       menucomplete \
       extendedglob \
       autoparamslash

autoload -Uz compinit

# If zsh completion cache was updated today
if test "$(date +'%j')" != "$(date -r ${ZDOTDIR:-$HOME}/.zcompdump +'%j')"; then
  compinit
else
  # Bypass the check for rebuilding the dump file and the usual call to compaudit
  compinit -C
fi
# }}}

# Aliases {{{
alias vim='nvim'
alias ls='ls -lAFh --color=always'
alias grep='grep --color=always'
alias c='xclip -selection clipboard'
alias gitted='git ls-files --error-unmatch'
alias t='clear; todo.sh -a -c -d ${XDG_CONFIG_HOME:-${HOME:-~}/.config}/.todo.actions.d/config'
alias \$=':;'
wim () { ${EDITOR} "$(which ${1:?No file selected...})" "${@:2}" }

# Dotfiles {{{
export DOTFILES_DIR="${XDG_CONFIG_HOME:-${HOME:-~}/.config}/dotfiles/.git"
export DOTFILES_TREE="${HOME:-~}"

alias dotted='dotfiles ls-files --error-unmatch'
alias dirtydots='dirtygit --git-dir "${DOTFILES_DIR}" --work-tree "${DOTFILES_TREE}" --git-add "-u"'
alias dots='dotfiles'
dotfiles () {
  if test "${#}" -eq "0"; then
    set - status
  fi
  dotfiles::no_add_dirs() {
    while test "${#}" -gt "0"; do
      if test -d "${1}"; then
        printf '%s\n' "Adding directories (${1}) is not permitted..."
        return 1
      fi
      shift
    done
  }
  if test "${1}" = "add"; then
    if ! dotfiles::no_add_dirs ${@:2}; then
      return
    fi
  fi
  git --git-dir="${DOTFILES_DIR}" --work-tree="${DOTFILES_TREE}" "${@}"
}
# }}}
# }}}

# Directory commands {{{
# https://gist.github.com/dofy/ff397555ae941987a123a81c879b5f8e
# conda_envs="$(conda info --envs | sed '/^#/d' | sed '/^base/d' | cut -d ' ' -f 1)"
# directory-autocmds() {
#   dirname="$(basename "$(pwd)")"
#   if printf '%s\n' "${conda_envs}" | grep -Fxq "${dirname}"; then
#     conda activate "${dirname}"
#   elif test "${CONDA_DEFAULT_ENV}" != "base"; then
#       conda activate base
#   fi
#   esac
# }
# add-zsh-hook chpwd directory-autocmds
# }}}

# {{{
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$("${HOME}/.local/share/miniconda3/bin/conda" 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "${HOME}/.local/share/miniconda3/etc/profile.d/conda.sh" ]; then
        . "${HOME}/.local/share/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="${HOME}/.local/share/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
