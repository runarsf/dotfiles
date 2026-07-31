{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.nix = {lib, ...}: let
    inherit (lib) mkDefault;
  in {
    imports = [
      self.nixosModules.legacyConsistency
    ];

    nix = {
      settings = rec {
        auto-optimise-store = true;
        warn-dirty = false;
        experimental-features = ["nix-command" "flakes" "pipe-operators"];
        substituters = [
          "https://cache.nixos.org/"
          "https://nix-community.cachix.org"
        ];
        trusted-substituters = substituters;
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
      };
      gc = {
        automatic = mkDefault true;
        dates = mkDefault "weekly";
        options = mkDefault "--delete-older-than 5d";
      };
    };
  };

  flake.homeModules.nix = {pkgs, ...}: {
    imports = [
      inputs.nix-index-database.homeModules.nix-index
    ];

    programs = {
      nix-index-database.comma.enable = true;
      direnv = {
        enable = true;
        nix-direnv.enable = true;
        config.global.hide_env_diff = true;
      };
    };

    home.sessionVariables = {
      NIXPKGS_ALLOW_UNFREE = "1";
    };

    home.file.".direnvrc".text =
      # bash
      ''
        # https://github.com/direnv/direnv/issues/73#issuecomment-152284914
        export_function() {
          local name=$1
          local alias_dir=$PWD/.direnv/aliases
          mkdir -p "$alias_dir"
          PATH_add "$alias_dir"
          local target="$alias_dir/$name"
          if declare -f "$name" >/dev/null; then
            echo "#!/usr/bin/env bash" > "$target"
            declare -f "$name" >> "$target" 2>/dev/null
            echo "$name" >> "$target"
            chmod +x "$target"
          fi
        }
      '';

    home.packages = with pkgs; [
      alejandra
      nixfmt
      nixd
      cached-nix-shell
      deadnix
      statix
      nix-inspect
      nix-output-monitor
      nvd
      nix-tree
      manix
    ];
  };

  flake.nixosModules.legacyConsistency = {lib, ...}: let
    inherit (lib) mapAttrs mapAttrs';
  in rec {
    nix = {
      nixPath = ["/etc/nix/path"];
      registry = mapAttrs (_: value: {flake = value;}) inputs;
    };

    environment.etc =
      mapAttrs' (key: value: {
        name = "nix/path/${key}";
        value.source = value.flake;
      })
      nix.registry;
  };

  flake.homeModules.niks = {
    pkgs,
    lib,
    config,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    options.features.niks = {
      flake = lib.mkOption {
        type = lib.types.str;
        default = "${config.home.homeDirectory}/.config/nixos";
        description = "Path to the NixOS flake, set as NH_FLAKE.";
      };
    };

    config = {
      home.packages = with pkgs; [
        self.packages.${system}.niks
        nh
      ];
      home.sessionVariables.NH_FLAKE = config.features.niks.flake;
    };
  };
}
