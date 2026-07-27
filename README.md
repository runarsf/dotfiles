# NixOS Configuration ❄️

## Getting Started

> [!NOTE]
> Before installing, it can be beneficial to add your user to `nix.settings.trusted-users` (e.g., `/etc/nixos/configuration.nix` if you just installed), as the `nh` wrapper sets the options `extra-substituters` and `extra-trusted-public-keys`, which are silently ignored if set by non-trusted users.
> This can significantly reduce the time needed for building.

First you will need an SSH key with access to the sops-protected vault and authentication access to your GitHub account.
Place it in `~/.ssh/id_nix`.

```nix
export NIX_CONFIG="extra-experimental-features = nix-command flakes pipe-operators"
eval $(ssh-agent); ssh-add ~/.ssh/id_nix

nix flake update vault
nix run .#niks -- os switch .#my-hostname

passwd
```

## Updating the Flake

The `updater` package can be used to update the flake.
It updates the flake inputs, locks the inputs defined in [`releaseLockedInputs`](./packages/updates/default.nix) to the latest release (including dependants), and updates instances of git fetchers[^fetchers] in the config.

```nix
nix run .#updater
```

The only caveat of this method is that fetchers[^fetchers] cannot be used in `let...in` expressions, as `update-nix-fetchgit`[^update-nix-fetchgit] cannot handle it.

[^fetchers]: https://ryantm.github.io/nixpkgs/builders/fetchers

[^update-nix-fetchgit]: https://github.com/expipiplus1/update-nix-fetchgit

### Manually Updating an Input

Some times you might want to manually update and input; in this case, use the unwrapped `update-flake` package.
This also works on other Nix flakes.

```nix
# Update Zed to the latest commit
nix run .#update-flake -- zed

# Update Zed to the latest release
nix run .#update-flake -- --release zed
```

## Module/Feature Structure

```nix
{
  self,
  inputs,
  lib',
  ...
}: {
  flake.nixosModules.myModule = {config, pkgs, lib, ...}: {
  };

  flake.homeModules.myModule = {config, pkgs, lib, ...}: {
  };

  perSystem = {
    pkgs,
    lib,
    self',
    ...
  }: {
    # https://birdeehub.github.io/nix-wrapper-modules/md/wrapper-modules.html
    packages.myPackage = inputs.wrapper-modules.wrappers.myPackage.wrap {
      inherit pkgs;
    };
  };
}
```
