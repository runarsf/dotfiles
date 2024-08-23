{
  # TODO https://github.com/astro/deadnix
  # TODO https://nixos.wiki/wiki/Storage_optimization
  # TODO https://github.com/symphorien/nix-du

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nur.url = "github:nix-community/nur";

    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # Used for umport, but updated version in utils
    # nypkgs = {
    #   url = "github:yunfachi/nypkgs";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixpkgs-wayland = {
      url = "github:nix-community/nixpkgs-wayland";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    hyprland = {
      type = "git";
      url = "https://github.com/hyprwm/Hyprland";
      submodules = true;
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    arkenfox = {
      url = "github:dwarfmaster/arkenfox-nixos";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    rednix.url = "github:redcode-labs/RedNix";

    stylix.url = "github:danth/stylix";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    nixvim.url = "github:runarsf/nixvim";

    vault = {
      url = "git+ssh://git@github.com/runarsf/vault";
      flake = false;
    };
  };

  outputs =
    inputs:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

    in
    rec {
      lib = import ./lib {
        inherit inputs;
        inherit (inputs.self) outputs;
      };

      nixosConfigurations.runix = lib.mkHost {
        system = "x86_64-linux";
        hostname = "runix";
        users = [ "runar" ];
      };

      nixosConfigurations.rpi = lib.mkHost {
        system = "aarch64-linux";
        hostname = "rpi";
        users = [ "runar" ];
      };

      nixosConfigurations.toaster = lib.mkHost {
        system = "x86_64-linux";
        hostname = "toaster";
        users = [ "thomas" ];
      };

      homeConfigurations.runar = lib.mkUser { username = "runar"; };
      homeConfigurations.thomas = lib.mkUser { username = "thomas"; };

      homeConfigurations."runar@runix" = lib.mkUser {
        username = "runar";
        system = "x86_64-linux";
        hostname = "runix";
      };

      homeConfigurations."runar@rpi" = lib.mkUser {
        username = "runar";
        system = "aarch64-linux";
        hostname = "rpi";
      };

      homeConfigurations."thomas@toaster" = lib.mkUser {
        username = "thomas";
        system = "x86_64-linux";
        hostname = "toaster";
      };

      formatter = lib.forEachSystem systems (pkgs: pkgs.nixfmt-rfc-style);
    };
}
