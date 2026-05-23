# Dotfiles

## Getting Started

```nix
export NIX_CONFIG="extra-experimental-features = nix-command flakes pipe-operators"
eval $(ssh-agent); ssh-add ~/.ssh/id_nix

nix flake update vault
nix run .#niks -- os switch .#my-hostname
```

## Updating the Flake

The `updater` package can be used to update the flake.
It updates the flake inputs, locks the inputs defined in [`releaseLockedInputs`](./packages/updates/default.nix) to the latest release (including dependants), and updates instances of git fetchers in the config.

```nix
nix run .#updater
```
