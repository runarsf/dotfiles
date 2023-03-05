#!/bin/sh

export SSH_ASKPASS="/usr/bin/ksshaskpass"

if ! pgrep -u "${USER}" ssh-agent > /dev/null; then
  ssh-agent > ~/.ssh-agent-info
fi
if test "${SSH_AGENT_PID}" == ""; then
  eval $(<~/.ssh-agent-info)
fi
