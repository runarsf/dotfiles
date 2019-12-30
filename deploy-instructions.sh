#!/usr/bin/env bash
# Though this is against shell-best-practices, `read` returns a non-zero code when reaching EOF, which means we can't exit on errors.
set +o errexit

# instructions.sh
# Deploy runs each line of  a heredoc (a set of instructions) formatted as linux commands.
# An instruction could be 'sudo apt install arch'.
# One instruction per line.
# This file is sourced by deploy.sh **with root privileges**, so don't do anything stupid like rm -rf / --no-preserve-root...

prefix='DEBIAN_FRONTEND=noninteractive sudo apt-get -yq install'
suffix='--no=install-recommends'

# Consider IFS='' before `read`.
# If you want to use tabs for readability, the line has to start with a tab character, not spaces.
read -r -d '' INSTRUCTIONS <<- EOINSTRUCTIONS
sudo apt-get update
zsh
git
curl
tmux
byobu
thefuck
vim
neovim
EOINSTRUCTIONS

# These changes should be in deploy.sh
# Prefix, suffix, and INSTRUCTIONS are set after this file is sourced.
: '
while read instruction; do
  case "${instruction}" in
    *\ * )
      ${instruction}
      ;;
    *)
      # Maybe this should use eval.
      # Write this to adapt the current deploy standard. (set -e; )
      ${prefix} ${instruction} ${suffix}
      ;;
  esac
done <<<$INSTRUCTIONS
'
