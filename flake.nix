{
  inputs = {
    # Use `nix run .#updater` to update
    # See `packages/updater/default.nix` for release-locked inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    vault = {
      url = "git+ssh://git@github.com/runarsf/vault";
      flake = false;
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    hypr-dynamic-cursors = {
      url = "github:VirtCode/hypr-dynamic-cursors";
      inputs.hyprland.follows = "hyprland";
    };
    nwg-displays.url = "github:nwg-piotr/nwg-displays";

    zed.url = "github:zed-industries/zed";

    vicinae = {
      url = "github:vicinaehq/vicinae";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    vicinae-extensions = {
      url = "github:vicinaehq/extensions";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.vicinae.follows = "vicinae";
    };

    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dms = {
      url = "github:AvengeMedia/DankMaterialShell";
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

    stackpkgs = {
      type = "git";
      url = "https://code.thishorsie.rocks/ryze/stackpkgs";
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";

    hytale-launcher.url = "github:JPyke3/hytale-launcher-nix";
  };

  outputs = inputs @ {
    self,
    import-tree,
    flake-parts,
    ...
  }: let
    inherit (builtins) elem;
    inherit (flake-parts.lib) mkFlake;
  in
    mkFlake {inherit inputs;} (
      import-tree.filterNot (
        x:
          elem (baseNameOf x) [
            "flake.nix"
            "treefmt.nix"
            "hardware-configuration.nix"
          ]
      )
      ./.
    );
}
