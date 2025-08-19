# NixOS Configuration ❄️

## Initial setup

1. Install [NixOS](https://nixos.org/download.html) (or Nix)
    > **NB!** Make sure `$USER`- and `$HOST`name match the configuration.

1. Put your SSH key with access to the sops-protected vault and authentication access to your GitHub account on your host (`~/.ssh/id_nix`),\
  and add it to the ssh agent
    ```bash
    eval $(ssh-agent)
    ssh-add ~/.ssh/id_nix
    ```

1. Clone this repository (to path matching [`$FLAKE`](./modules/users/development/nix.nix))
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
    ./packages/niks/niks.sh os switch --ask --hostname ${HOST:?} .
    ```

1. You should now be able to log in with the same username and the password set in the config (`changeme`).\
  After logging in, make sure to change your password, and check that all the [substituters](./modules/nix/nix.nix) are applied correctly.
    ```bash
    passwd

    nix config show | egrep "^substituters"
    ```


## Useful tools and resources

- [NixOS Package Search](https://search.nixos.org/packages)
- [NixOS Options Search](https://search.nixos.org/options)
- [Home Manager Option Search](https://home-manager-options.extranix.com/)
- [Noogle](https://noogle.dev/)
- [Nix Package Version Search](https://lazamar.co.uk/nix-versions)
- `sudo nix-store --verify --check-contents --repair`
- [Stuck because binary cache is down](https://discourse.nixos.org/t/i-seem-to-be-stuck-because-a-binary-cache-is-down/23641/3)\
    `sudo nixos-rebuild test --flake .# --accept-flake-config --option build-use-substitutes false`
- Prefetch hash (prepend `sha256:`): `nix-prefetch-url --unpack $url`
- Jump into the build of a derivation: `nix-shell -E 'with import <nixpkgs> {  }; callPackage ./default.nix {  }'`
- Test an expression: `nix eval -f test.nix fn`\
    <kbd>test.nix</kbd>
    ```
    { lib ? import <nixpkgs/lib> }:

    {
      fn = builtins.trace "Hello World!" true;
    }
    ```


> Core library functions are shamelessly stolen from [![avatar](https://images.weserv.nl/?url=avatars.githubusercontent.com/u/39416660?v=4&h=20&w=20&fit=cover&mask=circle&maxage=7d) `imatpot/dotfiles`](https://github.com/imatpot/dotfiles)
