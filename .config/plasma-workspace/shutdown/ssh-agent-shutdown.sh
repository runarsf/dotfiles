#!/bin/sh

test -z "${SSH_AGENT_PID}" || eval "$(ssh-agent -k)"
