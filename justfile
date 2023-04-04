#!/usr/bin/env -S just --justfile
# vim: set ft=just :

# just -f <(curl -fs https://raw.githubusercontent.com/runarsf/dotfiles/master/justfile) deploy

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

[private]
@default:
  printf '%s\n' "[$(whoami)@$(hostname)] ({{capitalize(distro)}})"
  {{just}} --list --unsorted --list-heading $'Commands:\n' --list-prefix '  '

# Runs justfile from dotfiles repository
@remote COMMAND *args:
  {{just_executable()}} --justfile <(curl -fs https://raw.githubusercontent.com/runarsf/dotfiles/master/justfile) {{COMMAND}} "${@}"

# Deploys dotfiles
[no-cd]
deploy:
  printf '%s\n' "WIP"

# Updates everything
@upgrade: update mouse-acceleration update-limits

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

# Disables mouse acceleration
mouse-acceleration:
	#!/usr/bin/env bash
	set -euo pipefail
	files=(
		"/etc/X11/xorg.conf.d/50-mouse-acceleration.conf"
		"/usr/share/X11/xorg.conf.d/50-mouse-acceleration.conf"
	)
	for file in "${files[@]}"; do
		if test ! -e "${file}"; then
			cat <<-'EOF' | sudo tee "${file}"
			Section "InputClass"
			    Identifier "My Mouse"
			    MatchIsPointer "yes"
			    Option "AccelerationProfile" "-1"
			    Option "AccelerationScheme" "none"
			    Option "AccelSpeed" "-1"
			EndSection
			EOF
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
pkg-dump PKGLIST=(config_dir / "dotfiles/pkgs" / distro):
  #!/usr/bin/env bash
  mkdir -p "$(dirname "{{PKGLIST}}")"
  case "{{distro}}" in
    arch|endeavouros)
      pacman -Qqen > {{PKGLIST}}
      pacman -Qqem > {{PKGLIST}}-aur;;
  esac

# Installs package list
pkg-restore PKGLIST=(config_dir / "dotfiles/pkgs" / distro):
  #!/usr/bin/env bash
  case "{{distro}}" in
    arch|endeavouros)
      sudo pacman -S --needed - < {{PKGLIST}}
      paru -S --needed - < {{PKGLIST}}-aur;;
  esac
