# NixOS Configuration ❄️

## Installation / initial build

```nix
# First, make sure your hostname matches the one in the config, and /etc/nixos/hardware-configuration.nix is copied to ./hosts/${HOST}/hardware-configuration.nix
sudo nixos-rebuild --flake .# --accept-flake-config switch

# Afterwards, start a new shell and verify that all substituters are in use
nix --extra-experimental-features nix-command show-config | egrep "^substituters"
```


## Useful tools and references

- [Home Manager Option search](https://home-manager-options.extranix.com/)
- [Nix package version search](https://lazamar.co.uk/nix-versions)
- Jump into the build of a derivation: `nix-shell -E 'with import <nixpkgs> {  }; callPackage ./default.nix {  }'`


> Core library functions are shamelessly stolen from [![avatar](https://images.weserv.nl/?url=avatars.githubusercontent.com/u/39416660?v=4&h=20&w=20&fit=cover&mask=circle&maxage=7d) `imatpot/dotfiles`](https://github.com/imatpot/dotfiles)
