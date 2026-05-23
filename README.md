## Getting Started

```nix
export NIX_CONFIG="extra-experimental-features = nix-command flakes pipe-operators"
eval $(ssh-agent); ssh-add ~/.ssh/id_nix

nix flake update vault
nix run .#niks -- os switch .#my-hostname
```
