# .files

## Prerequisites & requirements
- bash

## Set up
- git:
    - ``if [ ! -d ~/git/ ]; then mkdir ~/git; else echo '~/git/ already exists, ignoring.'; fi && if [ ! -d ~/git/dotfiles/ ]; then git clone https://github.com/runarsf/dotfiles.git ~/git/dotfiles; else echo '~/git/dotfiles/ already exists, nothing to do.'; fi``
- don't have git?
    - [Download](https://github.com/runarsf/dotfiles/archive/master.zip) the repo as a zip
    - Extract it (e.g. with `unzip`)
- `./deploy.sh --help` for more info

## Known issues
- `.xinitrc` does not work with Xbox One Wired controllers

## Installation
Old:
- Go to the folder containing the deploy script (typically ~/git/dotfiles/)
- `chmod +x deploy.sh`
- Read all the information
- `./deploy.sh --full` (see `./deploy.sh --help` for more options)
- Click *enter* on queries regarding aur packages
- When zsh opens, type `exit` and click *enter*, the installation will continue
- When the script is done, log out, and log in with `i3`
- Hotfix; urxvt colors: (automatically applied in deploy script):
    - ``sed -i "1 s/^.*$/#include \"\/home\/$USER\/.xres\/urxvt\"/" ~/.Xresources``
