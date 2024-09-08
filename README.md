# NixOS Configuration ❄️

## Installation / initial build

```nix
# First, make sure `hostname` is right and /etc/nixos/hardware-configuration.nix is copied to ./hosts/$(hostname)/hardware-configuration.nix

# Try this
sudo nixos-rebuild --flake .# --accept-flake-config switch

# If the previous is stuck building for a while, try this instead
nix build --substituters "https://hyprland.cachix.org https://cache.nixos.org" --trusted-public-keys "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc= hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ".#nixosConfigurations.$(hostname).config.system.build.toplevel"
sudo nixos-rebuild --flake .# switch

# Afterwards, start a new shell and verify that both hyprland.cachix.org and cache.nixos.org are substituters
nix --extra-experimental-features nix-command show-config | egrep "^substituters"
```


## Useful tools and references

- [Home Manager Option search](https://home-manager-options.extranix.com/)
- [Nix package version search](https://lazamar.co.uk/nix-versions)
- Jump into the build of a derivation: `nix-shell -E 'with import <nixpkgs> {  }; callPackage ./default.nix {  }'`


> Library functions are shamelessly stolen from [![avatar](https://images.weserv.nl/?url=avatars.githubusercontent.com/u/39416660?v=4&h=20&w=20&fit=cover&mask=circle&maxage=7d) `imatpot/dotfiles`](https://github.com/imatpot/dotfiles)
