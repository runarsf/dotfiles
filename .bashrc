# ========================================================================== #
#
# PERSONAL ~/.bashrc FILE for UBUNTU
#+ by Runar Fredagsvik [runarsf@protonmail.com]
#
# ========================================================================== #

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Personal Aliases
alias ..='cd ..'
alias cls='clear'
alias bashrc='vim ~/.bashrc'
alias reload='. ~/.bashrc'
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'
alias ls='ls -h --color'
alias lx='ls -lXB'         #  Sort by extension.
alias lk='ls -lSr'         #  Sort by size, biggest last.
alias lt='ls -ltr'         #  Sort by date, most recent last.
alias lc='ls -ltcr'        #  Sort by/show change time,most recent last.
alias lu='ls -ltur'        #  Sort by/show access time,most recent last.

# Check if nano installed
if [ $(dpkg-query -W -f='${Status}' zsh 2>/dev/null | grep -c "ok installed") -eq 0 ];
then
	sudo apt-get install zsh;
else
	echo zsh already installed
fi

if [ -f ~/.oh-my-zsh ]
then
    echo the file exists
fi
#sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# Separate alias file.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi