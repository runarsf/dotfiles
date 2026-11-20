# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This file lives at `users/runar/_config/claude/CLAUDE.md`. Home Manager links it to
`~/.config/nixos/CLAUDE.md`, so the copy at the repo root is a read-only store symlink.
Edit the source under `users/runar/_config/claude/`; the link updates on the next switch,
which the user runs.

## Commands

```nix
# Rebuild (nh wrapper; reads trusted substituters off the target config)
nix run .#niks -- os switch .#<hostname>
nix run .#niks -- os test .#<hostname>     # build without activating
nix run .#niks -- home switch .#<user>
nix run .#niks -- os boot .#<hostname>     # first install

# Eval/build a single config without switching
nix build .#nixosConfigurations.<hostname>.config.system.build.toplevel
nix build .#homeConfigurations.<user>.activationPackage
nix eval .#nixosConfigurations.<hostname>.config.<option>

nix build .#<package>       # anything in packages/
nix flake check
nix fmt                     # treefmt: nixfmt + deadnix

nix run .#update            # update inputs + git fetchers, re-lock release-pinned inputs
nix run .#update-flake -- --release <input>   # single input
```

Switching, booting and activating are the user's to run, never an agent's. The eval and build
commands above are the ones to use when checking your own work.

`niks` passes `--accept-flake-config` and the target host's substituters, so prefer it over bare `nixos-rebuild`/`nh`. Anything that evaluates sops or fonts needs `~/.ssh/id_nix` in a running `ssh-agent`, since `lib/vault.nix` fetches a private vault repo over SSH at eval time.

## Architecture

flake-parts + `import-tree`. Every `.nix` file under `parts/`, `modules/`, `packages/`, `hosts/`, `users/` is a flake module, auto-imported, no import lists to maintain. **import-tree skips any path whose basename starts with `_`** — that's how `_hardware.nix`, `_nixpkgs.nix`, `modules/hyprland/_animations/`, `users/*/_config/` stay out of the flake and get imported explicitly instead.

`flake.nix` splits its imports into `shared` (parts, modules, packages) and host/user trees. `shared` is also re-exported as `flake.flakeModules.default` so another flake can import the modules and declare its own hosts — see `SPLIT_PLAN.md`.

`lib/` is **not** read by import-tree. `flake.nix` imports it directly, so each `lib/*.nix` is a plain function returning an attrset. The merged result lands in two places: `lib'` as a module argument everywhere, and `flake.lib` (own helpers only). `lib'` also includes nixpkgs lib, home-manager lib and `nixlib`.

### Modules

Each file in `modules/` declares `flake.nixosModules.<name>` and/or `flake.homeModules.<name>`. **The name is camelCase and unrelated to the file path** (`modules/server/ssh.nix` → `sshServer`, `modules/development/android/` → `android`), and one file may declare several. Options go under `modules.<name>.*`.

`lib'.mkModule name module` wraps a module and adds a read-only `modules.<name>.enable = true`, so other modules can react with `config.modules.<name>.enable or false` without importing it. Pass `{ wants = [ ... ]; config = ...; }` to warn (never disable) when a companion module isn't enabled. A home module reaches nixos state through `osConfig`, not the other way round.

`lib'.mkService` (`lib/services.nix`) builds a container-backed service: a quadlet unit plus an nginx vhost under the wildcard cert, pulling in the `nginx` and `containers` modules. `modules/server/containers.nix` generates rootless podman quadlets for a fixed-uid service user (`users/blahaj`).

### Hosts and users

`parts/hosts.nix` and `parts/users.nix` declare the `hosts`, `users` and `serviceUsers` options; each host/user file just fills one entry in. Hosts become `nixosConfigurations.<name>`, users become `nixosModules.<name>` + `homeModules.<name>` + `homeConfigurations.<name>`.

A host sets `modules` (names, or attrsets of name → config, resolved by `lib'.useModules`), `configuration` (its own settings), and `extraModules` (things outside `modules/`, like `./_hardware.nix` or nixos-hardware). A user gets `defaultModules` from `parts/users.nix` unless `baseModules = false`, plus its own `modules` and `home`. Setting `root = ./.` makes `parts/users.nix` import every file in that user's `_config/`, each returning `{ homeModule?, nixosModule? }`.

`useModules` gives each import a stable `key`, so a module named by both a user and their host is evaluated once. Config in a module attrset is filtered against declared options, so the same name can appear with options it doesn't declare on the other side (nixos vs home).

`parts/host.nix` declares `host.desktop` (GUI-only modules gate on it) and `host.efi`.

### nixpkgs config

`pkgs` is instantiated once per system in `parts/default.nix`, so **`nixpkgs.config` cannot be set from a NixOS or home-manager module**. Add a `_nixpkgs.nix` next to the module returning a plain attrset (e.g. `{ allowedLicenses = [ ... ]; }`); `parts/default.nix` merges all of them into the `nixpkgsConfig` option. Hosts, users and importing flakes can set `nixpkgsConfig` directly.

### Packages

`packages/*/default.nix` define `perSystem.packages.<name>`. Nushell scripts are the norm for tooling (`writeNuBin` + `symlinkJoin` + `wrapProgram` for PATH).

## Conventions

- Git fetchers must be at top level, not inside `let ... in` — `update-nix-fetchgit` can't rewrite them. `# UPDATE:` comments are printed by `nix run .#update` for manual review.
- Release-pinned inputs are listed in `releaseLockedInputs` in `packages/update/default.nix`.
- `TODO.md` is the backlog, including modules still to port from `old-dotfiles/`.
