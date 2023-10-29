#!/usr/bin/env -S just --justfile
# vim: set ft=just :

# just -f <(curl -fs https://raw.githubusercontent.com/runarsf/dotfiles/master/justfile) deploy

# TODO use environment variables to configure gum
# TODO firefox

set positional-arguments

just := just_executable() + " --justfile " + justfile()
config_dir := env_var_or_default("XDG_CONFIG_HOME", env_var("HOME") / ".config")
distro := lowercase(`
  if test -f /etc/os-release; then
    . /etc/os-release
    printf '%s\n' "${ID_LIKE}"
  elif type lsb_release >/dev/null 2>&1; then
    lsb_release -is
  elif test -f /etc/lsb-release; then
    . /etc/lsb-release
    printf '%s\n' "${DISTRIB_ID}"
  elif test -f /etc/debian_version; then
    printf '%s\n' "debian"
  elif test -f /etc/SuSe-release; then
    printf '%s\n' "suse"
  elif test -f /etc/redhat-release; then
    printf '%s\n' "centos"
  else
    uname -s
  fi
  `)

dotfiles_repo := "git@github.com:runarsf/dotfiles.git"
dotfiles_tree := env_var("HOME")
dotfiles_dir  := config_dir / "dotfiles/.git"
dotfiles := 'git --git-dir="' + dotfiles_dir + '" --work-tree="' + dotfiles_tree + '"'

[private]
@default:
  printf '%s\n' "[$(whoami)@$(hostname)] ({{capitalize(distro)}})"
  {{just}} --list --unsorted --list-heading $'Commands:\n' --list-prefix '  '

[private]
@requires-tty:
  xhost >& /dev/null \
    && exit 1 \
    || exit 0

[private]
require *PKGS:
  #!/usr/bin/env bash
  # echo "Satisfying conditions by installing candidates: ${@}"
  case "{{distro}}" in
    arch|endeavouros)
      {{just}} require "paru"
      pkgs=""; aur=""
      while test "${#}" -gt "0"; do
        # Already installed
        pacman -Qi "${1}" >/dev/null 2>&1 \
          && shift \
          && continue
        # Is in the official repos
        pacman -Ssq "^${1}\$" >/dev/null 2>&1 \
          && pkgs="${pkgs} ${1}" \
          || aur="${aur} ${1}"
        shift
      done
      test -n "${pkgs}" && sudo pacman -S --needed ${pkgs}
      test -n "${aur}" && paru -S --needed ${aur}
      ;;
    *) printf '%s\n' "Unknown distribution {{distro}}, skipping package installation..."
  esac
  exit 0

# Runs justfile from dotfiles repository
@remote *args:
  {{just_executable()}} --justfile <(curl -fs https://raw.githubusercontent.com/runarsf/dotfiles/master/justfile) ${@}

# Deploys dotfiles
[linux]
deploy: update init-packages init-utils init-dotfiles init-startup-apps update-limits mouse-acceleration init-dm

[private]
init-utils: (require "git" "python3" "make" "zsh")
  mkdir -p "${HOME}/data"
  git clone git@github.com:runarsf/wallpapers.git "${HOME}/data/wallpapers"
  git clone git@github.com:runarsf/fonts.git "${HOME}/.local/share/fonts"
  fc-cache -vf
  python3 -m pip install --user --upgrade pynvim

  git config --global init.defaultBranch main

  git clone git@github.com:runarsf/screenshot.git "${HOME}/data/screenshot"
  sudo make -C "${HOME}/data/screenshot" install

  stack install --stack-yaml {{config_dir / "xmonad/stack.yaml"}}
  xmonad --recompile

  chsh -s /bin/zsh

[private]
init-packages:
  # TODO Split packages into categories
  sudo pacman -S --needed paru base-devel zsh alacritty rofi dmenu pass dunst vim neovim tmux trash-cli dnsutils acpi autorandr arandr lxrandr lxappearance python3 python-pip ghc firefox-developer-edition docker docker-compose podman podman-compose stack obsidian steam carla telegram-desktop audiorelay noto-fonts noto-fonts-extra noto-fonts-emoji yt-dlp easyeffects starship nitrogen spotify-launcher scrcpy btop bat ripgrep blueman lf gnome-keyring polkit-gnome just redshift jq yq qpwgraph lxinput trayer-srg wmctrl ffcast playerctl xorg-xsetroot xorg-server xorg-apps xorg-xinit xorg-xmessage libx11 libxft libxinerama libxrandr libxss pkgconf xdo xdotool xorg-xbacklight xprintidle xclip numlockx
  paru -S --needed getrid-git visual-studio-code-bin mullvad-vpn-beta-bin awesome-git yt-dlp-drop-in eww-git colorgrab picom-jonaburg-git ttf-font-awesome-4 dragon-drop wmutils-git betterlockscreen

[private]
init-keys: (require "git" "openssh" "gnupg")
  #!/usr/bin/env bash
  set -euo pipefail

  if test "$(find "${HOME}/.ssh" -type f -name "*.pub" | wc -l)" -le "0"; then
    ssh-keygen -t ssh-ed25519 -b 4096 -o -a 100 -C "$(gum input --header 'SSH key description' --placeholder '')"
  fi
  git config --global user.name >/dev/null \
    || git config --global user.name "$(gum input --header 'Git User Name' --placeholder '')"
  git config --global user.email >/dev/null \
    || git config --global user.email "$(gum input --header 'Git Email' --placeholder '')"

  gpg --import --allow-secret-key-import --pinentry-mode=loopback <(gum write --width 100 --height 10 --header 'GPG Private Key' --placeholder '' --char-limit=0)
  # TODO
  # gpg --edit-key <keyid> trust
  # pass init <keyid>
  # pass insert Screenshot/XBB_TOKEN

[private]
init-dotfiles: (require "git" "gum")
  #!/usr/bin/env bash
  set -euo pipefail

  if test -d '{{dotfiles_dir}}'; then
    printf '%s\n' "Dotfiles already deployed..."
    exit 0
  fi

  mkdir -p "$(dirname '{{dotfiles_dir}}')" '{{dotfiles_tree}}'
  git clone --bare '{{dotfiles_repo}}' '{{dotfiles_dir}}'
  {{dotfiles}} checkout

  {{dotfiles}} config --local status.showUntrackedFiles no
  {{dotfiles}} config --local core.hooksPath {{config_dir / "dotfiles/hooks"}}
  {{dotfiles}} config --local user.name "$(git config user.name || gum input --header 'Git User Name' --placeholder '')"
  {{dotfiles}} config --local user.email "$(git config user.email)"
  {{dotfiles}} config --local pull.rebase false

  {{dotfiles}} submodule update --init --recursive

[private]
init-dm: requires-tty
  #!/usr/bin/env bash
  set -euo pipefail

  display_managers=( "lightdm" "gdm" "lxdm" "sddm" )
  for dm in "${display_managers[@]}"; do
    sudo systemctl stop "${dm}" ||:
    sudo systemctl disable "${dm}" ||:
  done

  startx


# Updates system
update:
  #!/usr/bin/env bash
  set -euo pipefail

  case "{{distro}}" in
    arch|endeavouros)
      sudo pacman -Syu
      paru -Syu;;
    ubuntu|debian)
      sudo apt update
      sudo apt upgrade -y;;
  esac

# Modifies mouse acceleration
mouse-acceleration DISABLE="yes":
	#!/usr/bin/env bash
	set -euo pipefail

	files=(
		"/etc/X11/xorg.conf.d/50-mouse-acceleration.conf"
		"/usr/share/X11/xorg.conf.d/50-mouse-acceleration.conf"
	)
	for file in "${files[@]}"; do
		if test ! -e "${file}" -a "{{lowercase(DISABLE)}}" = "yes"; then
			printf '%s\n' "Creating ${file}"
			cat <<-'EOF' | sudo tee "${file}"
			Section "InputClass"
			    Identifier "My Mouse"
			    MatchIsPointer "yes"
			    Option "AccelerationProfile" "-1"
			    Option "AccelerationScheme" "none"
			    Option "AccelSpeed" "-1"
			EndSection
			EOF
		elif test -e "${file}" -a "{{lowercase(DISABLE)}}" = "no"; then
			printf '%s\n' "Deleting ${file}"
			sudo rm -f "${file}"
		fi
	done

# Updates /etc/security/limits.conf
update-limits:
  #!/usr/bin/env bash
  set -euo pipefail

  limits=(
    "@audio - nice -20"
    "@audio - rtprio 99"
    "forkbomb hard nproc 50"
  )
  for limit in "${limits[@]}"; do
    grep -qxF "${limit}" /etc/security/limits.conf \
      || printf '%s\n' "${limit}" \
        | sudo tee -a /etc/security/limits.conf
  done

# Dumps package list
# pkg-dump PKGLIST=(config_dir / "dotfiles/pkgs" / distro):
#   #!/usr/bin/env bash
#   set -euo pipefail
# 
#   mkdir -p "$(dirname "{{PKGLIST}}")"
#   case "{{distro}}" in
#     arch|endeavouros)
#       pacman -Qqen > {{PKGLIST}}
#       pacman -Qqem > {{PKGLIST}}-aur;;
#   esac

scroll DIRECTION CLASS="":
  #!/usr/bin/env bash
  set -euo pipefail

  case "{{DIRECTION}}" in
    up) BUTTON="4";;
    down) BUTTON="5";;
    *) printf '%s\n' "Invalid direction '{{DIRECTION}}'..."; exit 1;;
  esac

  wid=""
  if test -n "{{CLASS}}"; then
    wid="$(xdotool search --class '{{CLASS}}' | head -1)"
  fi
  export $(xdotool getwindowfocus getwindowgeometry --shell ${wid} | xargs)

  xdotool mousemove "$(( X + (WIDTH / 2) ))" "$(( Y + (HEIGHT / 2) ))"
  xdotool click "${BUTTON}"
  xdotool mousemove "$(( X + WIDTH - 10 ))" "$(( Y + 10 ))"

[private]
[no-cd]
[linux]
requires-gum:
  #!/usr/bin/env bash
  set -euo pipefail
  _gum_version="0.10.0"
  _gum_bin="${HOME:-~}/.local/bin/gum"
  if ! command -v gum >/dev/null 2>&1 && test ! -f "${_gum_bin}"; then
    pushd "$(dirname "${_gum_bin}")" >/dev/null
    wget -qO- "https://github.com/charmbracelet/gum/releases/download/v${_gum_version}/gum_${_gum_version}_Linux_x86_64.tar.gz" \
      | tar -xvz gum
  fi

[linux]
[private]
init-startup-apps AUTOSTART_FILE="${XDG_CONFIG_HOME:-${HOME:-~}/.config}/dotfiles/autostart": requires-gum
  #!/usr/bin/env bash
  set -euo pipefail
  applications="$(gum write --header 'Startup applications (CTRL-D to finish)' --placeholder 'Enter startup commands, one per line...')"
  if test -z "${applications// }"; then
    exit 0
  fi
  mkdir -p "$(dirname "{{AUTOSTART_FILE}}")"
  printf '%s\n' "${applications}" | awk 'NF{print $0 " &"}' >> "{{AUTOSTART_FILE}}"

[linux]
natural-scrolling:
  #!/usr/bin/env bash
  DEVICE_ID="$(xinput list | sed -rn 's/.+(touchpad|glidepoint)\s+id=([0-9]{1,2}).+/\2/pi')"
  PROP_ID="$(xinput list-props "${DEVICE_ID}" | sed -rn 's/.+Natural Scrolling Enabled \(([0-9]+)\).+/\1/pi')"
  xinput set-prop "${DEVICE_ID}" "${PROP_ID}" 1
  if test "${?}" -ne "0"; then
    printf '%s\n' "Failed to set natural scrolling..."
  else
    printf '%s\n' "Natural scrolling enabled..."
  fi

[linux]
audio:
  paru -S pipewire wireplumber

[linux]
wayland:
  paru -S hyprland-git
  paru -S hyprland-nvidia-git
  paru -S xdg-desktop-portal-hyprland-git xorg-xwayland extramaus wofi waybar wlprop wl-clip-persist-git gtklock cage greetd greetd-gtkgreet qt5-wayland qt6-wayland wl-clipboard hyprpicker xwaylandvideobridge-cursor-mode-2-git wev webcord jq swww hyprpaper nwg-look-bin
  pip install pyprland
  # git clone https://github.com/hyprwm/hyprland-plugins ~/.config/hypr-plugins
  # command = "cage -s -- gtkgreet" in /etc/greetd/config.toml
  # sudo systemctl enable greetd
  # add each login environment on a line to /etc/greetd/environments
