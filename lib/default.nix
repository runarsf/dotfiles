flake@{ inputs, ... }:

let
  core = import ./core.nix {
    inherit inputs;
    extlib = inputs.nixpkgs.lib.extend (_: _: inputs.home-manager.lib);
  };

in core.deepMerge [
  inputs.nixpkgs.lib
  inputs.home-manager.lib

  core

  (core.importAndMerge [
    ./conditionals.nix
    ./default-values.nix
    ./hosts.nix
    ./pkgs.nix
    ./secrets.nix
    ./users.nix
  ] flake)
]
