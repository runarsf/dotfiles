flake @ {inputs, ...}: let
  core = import ./core.nix {
    inherit inputs;
    extlib = with inputs; nixpkgs.lib // nix-darwin.lib // home-manager.lib // nixlib.lib;
  };
in
  with inputs;
    nixlib.lib.deepMerge [
      nixpkgs.lib
      nix-darwin.lib
      home-manager.lib
      nixlib.lib

      core

      (core.importAndMerge [
          ./conditionals.nix
          ./hosts.nix
          ./pkgs.nix
          ./state.nix
          ./users.nix
          ./utils.nix
          ./modules.nix
          ./wm.nix
        ]
        flake)
    ]
