{
  self,
  inputs,
  lib',
  ...
}: let
  features = lib'.useFeatures self [
    "sops"
    "ssh"
    "fonts"
    "zsh"
    "flatpak"
    "starship"
    "niri"
    # "hyprland"
    "hypridle"
    "hyprlock"
    "hyprpaper"
    {
      stylix = {
        wallpaper = ./wallpaper.jpg;
        cursor = "wii";
      };
    }
    "dms"
    "wezterm"
    "niks"
    "nix"
    "vicinae"
    "network"
    "cli"
    "git"
    "docker"
    "norwegian"
    "japanese"
    "pipewire"
    "zed"
    "zen"
    "discord"
    "steam" # TODO: put gaming and other device-specific features in respective hosts
    "controllers"
    {
      matrix = {
        clients = ["cinny"];
      };
    }
    "tuigreet"
  ];
in {
  flake.nixosModules.thomas = {
    pkgs,
    config,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    imports = features.nixos ++ [self.nixosModules.primaryUser];
    nix.settings.trusted-users = ["thomas"];
    features.ssh.keys = [
      {
        name = "id_priv";
        key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8glmBsdfxRsQxzZrljQynBF09jljQD4KIH33Kcx9Hw thoesp@protonmail.com";
      }
      {
        name = "id_nix";
        key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFw8lBpuv2bWKYxxXeeG6pZ7Ut2GCtjuEbuvVEp9DmeY nix";
      }
      {
        name = "id_ntnu";
        key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBYghkkwi+HG+q91Xhcdc+Ac8wYdIo8BzUZKUPa2/00f thomes@stud.ntnu.no";
      }
    ];
    home-manager.users.thomas = self.homeModules.thomas;
    users.users.thomas = {
      openssh.authorizedKeys.keys = map (k: k.key) config.features.ssh.keys;
      isNormalUser = true;
      shell = self.packages.${system}.zsh;
      home = "/home/thomas";
      initialPassword = "changeme";
      description = "Thomas Espervik";
      extraGroups = [
        "wheel"
        "networkmanager"
        "docker"
        "audio"
        "video"
        "libvirtd"
        "input"
        "i2c"
      ];
    };
  };

  flake.homeModules.thomas = {pkgs, ...}: {
    imports = features.home;

    home.packages = with pkgs; [television];

    home.stateVersion = "24.11";

    programs.git = {
      settings = {
        user = {
          name = "Thomas Espervik";
          email = "thoesp@protonmail.com";
        };
      };
    };
  };

  flake.homeConfigurations.thomas = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {system = "x86_64-linux";};
    extraSpecialArgs = {inherit self;};
    modules = [
      self.homeModules.thomas
      {
        home = {
          username = "thomas";
          homeDirectory = "/home/thomas";
        };
      }
    ];
  };
}
