#!/bin/bash

# Add this scripts as a Login Script using System Settings → Startup and Shutdown → Autostart.

# Wait for kwallet
kwallet-query -l kdewallet > /dev/null

for KEY in $(ls $HOME/.ssh/id_rsa_* | grep -v \.pub); do
  ssh-add -q ${KEY} </dev/null
done

for KEY in $(ls $HOME/.ssh/id_ed25519_* | grep -v \.pub); do
  ssh-add -q ${KEY} </dev/null
done
