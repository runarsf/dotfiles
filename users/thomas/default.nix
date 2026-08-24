{
  self,
  lib',
  ...
}: let
  features = lib'.useFeatures self [
    {
      sops = {
        privateKeys = ["id_priv" "id_ntnu"];
      };
    }
    {
      ssh = {
        keys = [
          {
            name = "id_priv";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8glmBsdfxRsQxzZrljQynBF09jljQD4KIH33Kcx9Hw";
          }
          {
            name = "id_nix";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFw8lBpuv2bWKYxxXeeG6pZ7Ut2GCtjuEbuvVEp9DmeY";
          }
          {
            name = "id_ntnu";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBYghkkwi+HG+q91Xhcdc+Ac8wYdIo8BzUZKUPa2/00f";
          }
        ];
      };
    }
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
    "yazi"
    "nixvim"
    "fastfetch"
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
    {
      matrix = {
        clients = ["cinny"];
      };
    }
    "tuigreet"
  ];
in {
  flake.nixosModules.thomas = lib'.mkNixosUser {
    inherit self;
    username = "thomas";
    features = features.nixos;
  };

  flake.homeModules.thomas = {pkgs, ...}: {
    imports = features.home;

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

  flake.homeConfigurations.thomas = lib'.mkUser {
    inherit self;
    username = "thomas";
    homeModule = self.homeModules.thomas;
  };
}
