# NixOS Configuration ❄️

## Initial setup

1. Install [NixOS](https://nixos.org/download.html) (or Nix)
  > [!CAUTION]
  > Make sure to set the correct user- and hostname.

1. Put your SSH key with access to the sops-protected vault and your GitHub account on your host (`~/.ssh/id_nix`),\
  and add it to the ssh agent
    ```bash
    eval $(ssh-agent)
    ssh-add ~/.ssh/id_nix
    ```

1. Clone this repository (to path matching [`$FLAKE`](./modules/users/development/nix.nix#L5))
    ```bash
    git clone git@github.com:runarsf/dotfiles.git ~/.config/nixos
    cd ~/.config/nixos
    ```

1. Copy `hardware-configuration.nix` to the host configuration
    ```bash
    cp /etc/nixos/hardware-configuration.nix ./hosts/${HOST}/hardware-configuration.nix
    ```

1. Build and switch
    ```bash
    # NB! If your hostname doesn't match the configured one, temporarily change it in the shell
    HOST=myhostname

    ./packages/niks/niks.sh os switch --ask --hostname ${HOST} . -- --accept-flake-config --extra-experimental-features 'flakes nix-command pipe-operator'
    ```

1. You should now be able to log in with the same username and the password set in the config (`changeme`).\
  After logging in, make sure to change your password, and check that all the substituters are applied correctly.
    ```bash
    passwd

    nix config show | egrep "^substituters"
    ```


## Useful tools and references

- [Home Manager Option search](https://home-manager-options.extranix.com/)
- [Nix package version search](https://lazamar.co.uk/nix-versions)
- Jump into the build of a derivation: `nix-shell -E 'with import <nixpkgs> {  }; callPackage ./default.nix {  }'`


> Core library functions are shamelessly stolen from [![avatar](https://images.weserv.nl/?url=avatars.githubusercontent.com/u/39416660?v=4&h=20&w=20&fit=cover&mask=circle&maxage=7d) `imatpot/dotfiles`](https://github.com/imatpot/dotfiles)
