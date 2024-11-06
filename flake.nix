{
  # NOTE https://github.com/astro/deadnix
  # NOTE https://github.com/symphorien/nix-du
  # TODO https://nixos.wiki/wiki/Storage_optimization
  # TODO Check based things in https://github.com/Misterio77/nix-starter-configs

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

    alien.url = "github:thiagokokada/nix-alien";

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

    wezterm.url = "github:wez/wezterm?dir=nix";

    hyprland = {
      type = "git";
      url = "https://github.com/hyprwm/Hyprland";
      submodules = true;
    };
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    nix-flatpak.url = "github:gmodena/nix-flatpak";

    stackpkgs = {
      type = "git";
      url = "https://code.thishorsie.rocks/ryze/stackpkgs";
    };

    zen-browser.url = "github:MarceColl/zen-browser-flake";

    arkenfox = {
      url = "github:dwarfmaster/arkenfox-nixos";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    stylix.url = "github:danth/stylix";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    nixvim.url = "github:runarsf/nixvim";

    vault = {
      url = "git+ssh://git@github.com/runarsf/vault";
      flake = false;
    };
  };

  outputs = inputs:
    let systems = [ "x86_64-linux" "aarch64-linux" ];

    in rec {
      lib = import ./lib {
        inherit inputs;
        inherit (inputs.self) outputs;
      };

      nixosConfigurations = {
        # TODO Should isDesktop be an option to mkHost?
        runix = lib.mkHost {
          system = "x86_64-linux";
          # TODO graphical = true;
          hostname = "runix";
          users = [ "runar" ];
        };

        rpi = lib.mkHost {
          system = "aarch64-linux";
          hostname = "rpi";
          users = [ "runar" ];
        };

        boiler = lib.mkHost {
          system = "x86_64-linux";
          hostname = "boiler";
          users = [ "thomas" ];
        };

        toaster = lib.mkHost {
          system = "x86_64-linux";
          hostname = "toaster";
          users = [ "thomas" ];
        };
      };

      homeConfigurations = {
        runar = lib.mkUser { username = "runar"; };

        "runar@runix" = lib.mkUser {
          username = "runar";
          system = "x86_64-linux";
          hostname = "runix";
        };

        "runar@rpi" = lib.mkUser {
          username = "runar";
          system = "aarch64-linux";
          hostname = "rpi";
        };

        thomas = lib.mkUser { username = "thomas"; };

        "thomas@boiler" = lib.mkUser {
          username = "thomas";
          system = "x86_64-linux";
          hostname = "boiler";
        };

        "thomas@toaster" = lib.mkUser {
          username = "thomas";
          system = "x86_64-linux";
          hostname = "toaster";
        };
      };

      formatter = lib.forEachSystem systems (pkgs: pkgs.nixfmt);
    };
}
