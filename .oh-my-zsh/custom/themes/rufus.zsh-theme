# rufus.zsh-theme

if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="green"; fi
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"



# colors
eval COLOR_GRAY='$FG[237]'
eval COLOR_ORANGE='$FG[214]'

# git settings
#ZSH_THEME_GIT_PROMPT_PREFIX="$FG[075]($FG[078]"
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[red]%}["

#ZSH_THEME_GIT_PROMPT_SUFFIX="$FG[075]) %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[red]%}] %{$reset_color%}"

ZSH_THEME_GIT_PROMPT_CLEAN=""
#ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ☀" # Ⓞ

ZSH_THEME_GIT_PROMPT_DIRTY="$COLOR_ORANGE*%{$reset_color%}"
#ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%} ☂" # Ⓓ

ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%} ⚡"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[cyan]%} ✭" # ⓣ
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[cyan]%} ✚" # ⓐ
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[red]%} ✖" # ⓧ
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[blue]%} ➜" # ⓡ
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[magenta]%} ♒" # ⓤ
ZSH_THEME_GIT_PROMPT_AHEAD="%{$fg[blue]%} |>" # 𝝙

# c >> folder
# d >> full path
# e >> ? (displays number, 0)
# h >> total command count? (increments on cmd execution)
# i >> session command count? (increments on cmd execution, starts new on new session)
# j >> ? (displays number, 0)
# l >> [pts/4]
# m >> computer name
# n >> username
# t >>  time
# w >> Date...? (Wed 26)
# x >> shell?
# y >> [pts/4]

repeatChar() {
    input="$1"
    count="$2"
    for (( i = 0; i < `tput cols`-`pwd | wc -c` - 6; i++ )); do
        printf "$input"
    done
}

PROMPT='$FG[237]-[ %d ]`repeatChar - 72` %{$reset_color%}
%{$fg[magenta]%}[%c] %{$reset_color%}'
#RPROMPT='${time} %{$fg[magenta]%}$(git_prompt_info)%{$reset_color%}'
RPROMPT='${time} $(git_prompt_info)$(git_prompt_status)$(git_prompt_ahead)%{$reset_color%}'

# PROMPT='$FG[237]----------------------------------------------------------------%{$reset_color%}
# $FG[032]%~\
# $(git_prompt_info) \
# $FG[105]%(!.#.»)%{$reset_color%} '
# PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
# RPS1='${return_code}'

# primary prompt

# right prompt
#if type "virtualenv_prompt_info" > /dev/null
#then
#    RPROMPT='$(virtualenv_prompt_info)$COLOR_GRAY%n@%m%{$reset_color%}%'
#else
#    RPROMPT='$COLOR_GRAY%n@%m%{$reset_color%}%'
#fi

# local time, color coded by last return code
time_enabled="%(?.%{$fg[green]%}.%{$fg[red]%})%*%{$reset_color%}"
time_disabled="%{$fg[green]%}%*%{$reset_color%}"
time=$time_enabled