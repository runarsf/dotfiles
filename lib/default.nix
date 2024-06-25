flake@{ inputs, ... }:

let
  core = import ./core.nix {
    inherit inputs;
    extlib = with inputs; nixpkgs.lib // nix-darwin.lib // home-manager.lib;
  };

in core.deepMerge [
  inputs.nixpkgs.lib
  inputs.nix-darwin.lib
  inputs.home-manager.lib

  core

  (core.importAndMerge [
    ./conditionals.nix
    ./hosts.nix
    ./pkgs.nix
    ./secrets.nix
    ./state.nix
    ./users.nix
    ./utils.nix
  ] flake)
]
