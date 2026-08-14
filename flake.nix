{
  inputs = {
    # Use `nix run .#updater` to update
    # See `packages/updater/default.nix` for release-locked inputs
    # NOTE: adding inputs.nixpkgs.follows will likely make cache miss
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

    nixvim.url = "github:runarsf/nixvim";

    hyprland.url = "github:hyprwm/Hyprland/275e27704a36d956fbdc28cec6399b8e298b06ca";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins/v0.56.0";
      inputs.hyprland.follows = "hyprland";
    };
    hypr-dynamic-cursors = {
      url = "github:VirtCode/hypr-dynamic-cursors";
      inputs.hyprland.follows = "hyprland";
    };

    niri-scratchpad.url = "github:argosnothing/niri-scratchpad";

    nwg-displays.url = "github:nwg-piotr/nwg-displays/v0.4.3";

    zed.url = "github:zed-industries/zed/v1.13.2";

    vicinae.url = "github:vicinaehq/vicinae/v0.24.0";
    vicinae-extensions = {
      url = "github:vicinaehq/extensions";
      inputs.vicinae.follows = "vicinae";
    };

    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dms = {
      url = "github:AvengeMedia/DankMaterialShell/v1.5.3";
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
            "nixpkgs.nix"
          ]
      )
      ./.
    );
}
