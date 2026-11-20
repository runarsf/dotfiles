{
  inputs,
  lib,
  lib',
  config,
  ...
}:
let
  inherit (lib) mkOption types;

  # _nixpkgs.nix files next to modules, e.g. a license a module needs accepted.
  moduleNixpkgsConfigs = map import (
    builtins.filter (p: builtins.baseNameOf p == "_nixpkgs.nix") (
      lib'.concatPaths {
        paths = ../modules;
        filterDefault = false;
      }
    )
  );
in
{
  imports = [
    inputs.home-manager.flakeModules.home-manager
    inputs.treefmt-nix.flakeModule
  ];

  # pkgs is built once per system and shared by every host and user, so
  # nixpkgs.config can't be set from NixOS or home-manager modules.
  options.nixpkgsConfig = mkOption {
    type = types.submodule {
      freeformType = types.attrsOf types.anything;
      options.permittedInsecurePackages = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
    };
    default = { };
    description = "nixpkgs config for every system.";
  };

  config = {
    nixpkgsConfig = lib.mkMerge (
      [
        {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        }
      ]
      ++ moduleNixpkgsConfigs
    );

    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    perSystem =
      { system, ... }:
      {
        treefmt.imports = [ ../_treefmt.nix ];
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          config = config.nixpkgsConfig;

          overlays = [
            (_: prev: {
              nur = import inputs.nur {
                pkgs = prev;
                nurpkgs = import inputs.nixpkgs { inherit (prev) system config overlays; };
              };
            })
          ];
        };
      };
  };
}
