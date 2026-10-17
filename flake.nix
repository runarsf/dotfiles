{
  inputs = {
    # Use `nix run .#updater` to update
    # See `packages/updater/default.nix` for release-locked inputs
    # NOTE: adding inputs.nixpkgs.follows will likely make cache miss
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.zst";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-multiverse.url = "github:fzakaria/nixpkgs-multiverse";

    alien.url = "github:thiagokokada/nix-alien";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    import-tree.url = "github:vic/import-tree";
    flake-parts.url = "github:hercules-ci/flake-parts";
    wrapper-modules = {
      url = "github:BirdeeHub/nix-wrapper-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixlib = {
      url = "github:runarsf/nixlib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim.url = "github:runarsf/nixvim";

    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    hypr-dynamic-cursors = {
      url = "github:VirtCode/hypr-dynamic-cursors";
      inputs.hyprland.follows = "hyprland";
    };

    nwg-displays.url = "github:nwg-piotr/nwg-displays/v0.4.4";

    zed.url = "github:zed-industries/zed/v1.24.2";

    vicinae.url = "github:vicinaehq/vicinae/v0.29.1";
    vicinae-extensions = {
      url = "github:vicinaehq/extensions";
      inputs.vicinae.follows = "vicinae";
    };

    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dms = {
      url = "github:AvengeMedia/DankMaterialShell/v1.6.3";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dgop = {
      url = "github:AvengeMedia/dgop";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dms-plugin-registry = {
      url = "github:AvengeMedia/dms-plugin-registry";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-flatpak.url = "github:gmodena/nix-flatpak";

    stackpkgs = {
      type = "git";
      url = "https://code.thishorsie.rocks/ryze/stackpkgs";
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";

    hytale-launcher.url = "github:JPyke3/hytale-launcher-nix";

    helium = {
      url = "github:schembriaiden/helium-browser-nix-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      import-tree,
      flake-parts,
      ...
    }:
    let
      inherit (builtins) elem;
      inherit (flake-parts.lib) mkFlake;

      # lib/ is read here instead of by import-tree, so its files are plain
      # functions rather than flake modules.
      myLib = import ./lib { inherit inputs; };
    in
    mkFlake
      {
        inherit inputs;
        specialArgs = { inherit (myLib) lib'; };
      }
      {
        imports = [
          (import-tree.filterNot (
            x:
            elem (baseNameOf x) [
              "flake.nix"
              "treefmt.nix"
              "hardware-configuration.nix"
              "nixpkgs.nix"
            ]
            || baseNameOf (dirOf x) == "lib"
          ) ./.)
        ];

        flake.lib = myLib.own;
      };
}
