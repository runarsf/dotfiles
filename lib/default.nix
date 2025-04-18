flake @ {inputs, ...}: let
  core = import ./core.nix {
    inherit inputs;
    extlib = with inputs; nixpkgs.lib // nix-darwin.lib // home-manager.lib;
  };
in
  with inputs;
    core.deepMerge [
      nixpkgs.lib
      nix-darwin.lib
      home-manager.lib
      nixlib.lib

      core

      (core.importAndMerge [
          ./conditionals.nix
          ./hosts.nix
          ./pkgs.nix
          ./secrets.nix
          ./state.nix
          ./users.nix
          ./utils.nix
          ./modules.nix
        ]
        flake)
    ]
