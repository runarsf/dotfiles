{
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

    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    arkenfox = {
      url = "github:dwarfmaster/arkenfox-nixos";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";

    rednix.url = "github:redcode-labs/RedNix";

    nixvim.url = "github:runarsf/nixvim";

    sf-mono-liga-src = {
      url = "github:shaunsingh/SFMono-Nerd-Font-Ligaturized";
      flake = false;
    };

    vault = {
      url = "git+ssh://git@github.com/runarsf/vault";
      flake = false;
    };
  };

  outputs = inputs:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];

    in rec {
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

      homeConfigurations.runar = lib.mkUser { username = "runar"; };

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

      formatter = lib.forEachSystem systems (pkgs: pkgs.nixfmt-rfc-style);
    };
}
