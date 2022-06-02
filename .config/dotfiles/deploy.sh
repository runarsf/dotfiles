#!/bin/sh
set -o errexit
set -o nounset

: "${GIT_USER:=runarsf}"
: "${GIT_REPO:=git@github.com:${GIT_USER}/dotfiles.git}"
: "${SSH_KEY:=${HOME}/.ssh/id_rsa}"
: "${CHEZMOI:=${HOME}/.local/bin/chezmoi}"
: "${CHEZMOI_DIR:=$(readlink -m $(dirname "${CHEZMOI}"))}"

set -o allexport
test -f /etc/os-release && source /etc/os-release
set +o allexport

confirm () { # <message:str> {{{
  local -r msg="${1:-Continue?}"
  printf '%s' "${msg} [Y/n]"
  while :; do
    read -p ' ' -n 1 -r REPLY </dev/tty
    printf '\n'
    case "${REPLY}" in
      [Yy]*|'') return 0;;
      [Nn]*) return 1;;
      *) printf '%s ' 'Please answer [Y]es or [N]o...';;
    esac
  done
} # }}}

log () { # <level:str> <message:str> {{{
  local -r LOG_LEVEL="${1}"
  shift
  local -r LOG_MESSAGE="${@}"
  printf '%s\n' "${LOG_LEVEL^^} ${LOG_MESSAGE}"
} # }}}

log INFO "System detected: ${ID_LIKE:=none}"
if confirm 'Update and install packages?'; then
  case "${ID_LIKE:-}" in
    arch)
      sudo pacman -Syu
      sudo pacman -S paru zsh alacritty wezterm-nightly-bin bspwm sxhkd rofi base-devel pass dunst vim neovim polybar tmux ueberzug trash-cli highlight dnsutils acpi autorandr arandr lxrandr lxappearance python3 python-pip playerctl jq docker docker-compose xorg-xsetroot xdo xdotool xorg-xbacklight xprintidle xclip numlockx wmctrl network-manager-applet trayer-srg starship ripgrep zoxide # mediainfo texlive-most biber texlive-bibtexextra
      paru -Syu
      paru -S wmutils-git dragon-drop ttf-font-awesome-4 lf picom-jonaburg-git spotify betterlockscreen bsp-layout eww-git awesome-git # tllocalmgr-git glow colorgrab ttf-font-awesome-4
      python3 -m pip install 'xonsh[full]'
      ;;
  esac
fi

if test ! -x "$(command -v chezmoi)" \
     -a ! -e "${CHEZMOI}"; then
  log INFO "Chezmoi not installed, setting up in ${CHEZMOI_DIR}..."

  mkdir -p "${CHEZMOI_DIR}"
  sh -c "$(curl -fsLS https://raw.githubusercontent.com/twpayne/chezmoi/master/assets/scripts/install.sh)" -- -b "${CHEZMOI_DIR}"
else
  if test -x "$(command -v chezmoi)"; then
    CHEZMOI="$(command -v chezmoi)"
    CHEZMOI_DIR="$(readlink -m $(dirname "${CHEZMOI}"))"
  fi

  log INFO "Chezmoi already installed in ${CHEZMOI_DIR}..."
fi
readonly CHEZMOI CHEZMOI_DIR

if test ! -f "${SSH_KEY}"; then
  while :; do
    read -p 'SSH-key not found, do you want to create one? [y/n] ' ans
    case "${ans}" in
      [Yy]*) break;;
      [Nn]*) printf '%s\n' "Alright, provide a private key-path with \$SSH_KEY or put one in ${SSH_KEY}"; exit 1;;
      *) printf '%s\n' 'Please answer Yes or No...';;
    esac
  done

  log INFO 'Generating SSH key...'
  ssh-keygen -f "${SSH_KEY}" -t rsa -b 4096 -o -a 100 -C "${GIT_USER}"

  printf 'Plase make sure the public key is added as an SSH/deploy key to your git provider...\n\n'
  cat "${SSH_KEY}.pub"
  printf '\n'

  printf 'Press [Return] to continue...\n'
  read ans
fi

eval "$(ssh-agent)"
ssh-add "${SSH_KEY}"

"${CHEZMOI}" init --apply --ssh "${GIT_REPO}"
if confirm 'Dotfiles checked out, apply now?'; then
  "${CHEZMOI}" apply
fi
