- Fix proper font in wezterm
- wezterm theme looks incorrect
- rewrite hyprland scripts to nushell
- fix commented out binds in hyprland
- configure default terminal
- fix hyprland switch workspace bind
- check what new things were added to the config (sine )
- remove display manager config from host config and unify into module
- maybe split locales into separate display/locale module (norwegian locale with english display)
- set default hostname and stateversion (like in modules/linux/default-config.nix, maybe a mkHost or something)
- figure out a way to do desktop modules (hyprland shouldn't be on anuc)
- maybe mkUser so we can add trusted-users
- add user to docker group dynamically
- create presets with common config (desktop, laptop, server, ...)
- noctalia: avatar and font in noctalia
- hyprland: start noctalia/dms/vicinae with hyprland
- pipewire: add user to pipewire group in pipewire feature
- ctf: find a better way to check if android module is enabled
- ssh: users.extraUsers.root.openssh.authorizedKeys.keys
- nginx: is it cleaner to have lib/nginx.nix in nginx module?
- vault: document required secrets. either here or in vault. nginx, sops, fonts, ...
- nginx: https://nixos.wiki/wiki/Nginx#Hardened_setup_with_TLS_and_HSTS_preloading
- naming: rename features to modules?
- naming: rename features/server/ to features/services/?
- generally restructure features, is very messy
- maybe move starship out of zsh module into a generic shell module? or maybe shell/zsh.nix, shell/nushell.nix, shell/starship.nix, etc.
- research: https://den.denful.dev/
- research: https://github.com/feel-co/hjem
- the primaryUser feature is a bit whack, maybe move this somewhere else
- less boilerplate; setting primaryUser and trusted-users for each user is a bit unfortunate (see users/runar/default.nix)
- importing primaryUser in users/runar/default.nix is whack, can we not make it automatically be there?
- openssh.authorizedKeys.keys in ssh.nix instead of runar/default.nix
- look at wrapper modules and rewrite what can be https://birdeehub.github.io/nix-wrapper-modules/md/wrapper-modules.html
- ssh: services.ssh-agent.enable
- zsh: make sure zocixe works in zsh since zsh is no longer in home manager
- how would we have machine-specific home manager modules enabled? e.g., only webcam on runar@runix
- consider whether features like wayland, dev.make, printing, etc. can be in a common module (e.g., make can be in cli or c, wayland and printing in desktop, etc.)
- where does one properly add allowUnfree?
- Add user to trusted users `nix.settings.trusted-users = [ "root" "@wheel" ];`
- flake udpater: make it lock with the tag name (e.g., v2.3.1) instead of the commit hash of that release
- how to configure home manager modules that should only be active on a certain host? e.g., gaming-related modules that need home manager config. how would i configure that for the modelland host
- gitui
- zsh not saving history
- why doesn't opentabletdriver start automatically
- does hyprland bind to open vicinae belong in hyprland or vicinae config? same applies for terminal
- rework zsh config to a more stable format
- why isn't $NH_FLAKE available in the environment?

## Hosts (separate repos)
- [ ] `boiler` — thomas's gaming desktop
- [ ] `toaster` — thomas's gaming desktop
- [ ] `hsrv` — legacy server (minimal config, may just fold into `anuc` instead of porting)
- [ ] `rpi` — Raspberry Pi host
- [x] `anuc`, `runix` (plus new `vm`)

## Users
- [ ] `thomas` — user + home-manager config not created yet
- [x] `runar`, `blahaj` (system user only)

## Server / self-hosted services
- [x] `containers.nix` quadlet framework, `glance` service
- [ ] Wire `containers`/`nginx`/`fail2ban`/`teleport` into the `anuc` host — modules exist but aren't imported anywhere yet
- [ ] Container services: `jellyfin`, `immich`, `slskd`, `wrtag`, `copyparty`, `freshrss`, `mealie`, `solidtime`, `stremio` (server variant), `wastebin`, `gonic`, `pinchflat`

## Development environments
- [ ] `neovim`
- [ ] `vscode`
- [ ] `rust`
- [ ] `java`
- [ ] `javascript`
- [ ] `iac`
- [ ] `python`
- [ ] `qmk` (+ udev rules)
- [ ] `nix` dev tooling (nixd/alejandra, distinct from the `nix.nix` feature)

## Desktop apps / WM extras
- [ ] `gpg`
- [ ] `keychain`
- [ ] `spotify`
- [ ] `logitech`
- [ ] `localsend`
- [ ] `kvm`
- [ ] `mullvad`
- [ ] `signal`
- [ ] `virtualbox`
- [ ] `webdav`
- [ ] `nuke` (home-manager pre-clean utility)
- [ ] `marker` (OCR container, low priority)
- [ ] Terminal alternatives: `alacritty`, `kitty`, `ghostty` (wezterm already ported)
- [ ] Possibly superseded, verify before porting: `dunst`, `waybar`, `ulauncher`, `lf`, `trayer`
- [ ] `wayle`, `ambxst` — look unfinished/abandoned in old-config; probably skip

## Gaming
- [x] `steam`, `ffxiv`, `hytale`, `minecraft`, `emulation`, `controllers`
- [ ] Wire dualsense/8bitdo udev rules

## Locales
- [x] `norwegian`
- [ ] `japanese`

## System / linux-level modules
- [ ] `firewall`
- [ ] `thunderbolt`
- [ ] `virtualisation` (libvirt/kvm at system level)
- [ ] `legacy-consistency`
- [ ] `service-account` (generic ops-style system user, generalize from `blahaj`)
- [ ] `mkUser`/`mkHost` helper for default hostname + stateVersion (already noted in TODO.md)
