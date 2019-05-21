#!/bin/sh
# Profile file. Runs on login.

export PATH=$PATH:$HOME/bin/:"`ruby -e 'puts Gem.user_dir'`/bin:$PATH"

export EDITOR="vim"
export VISUAL="$EDITOR"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export FILE="vim"
