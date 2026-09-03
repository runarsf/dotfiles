{
  self,
  lib',
  inputs,
  ...
}: let
  features = lib'.useFeatures self [
    {
      sops = {
        privateKeys = ["id_priv" "id_golog"];
      };
    }
    {
      ssh = {
        keys = [
          {
            name = "id_priv";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBT5zQFdVRooe5SfFZ2gKpruHF7FTw1OycTczRrLsR+M";
          }
          {
            name = "id_nix";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z";
          }
          {
            name = "id_golog";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMj5BHJ789MPmWsYNLCcfstrH5ouqr9LrkqvL3pKcvb5";
          }
        ];
      };
    }
    "git"
    "podman"
    "discord"
    "fonts"
    "zsh"
    "starship"
    "wayland"
    "nixvim"
    "niri"
    "nushell"
    "hyprland"
    "hypridle"
    "hyprlock"
    "hyprpaper"
    "xdg"
    {
      stylix = {
        wallpaper = ./outerwilds.jpeg;
        cursor = "wii";
      };
    }
    {
      dms = {
        aiUsage = true;
      };
    }
    "wezterm"
    "niks"
    "nix"
    "vicinae"
    "network"
    "cli"
    "norwegian"
    "pipewire"
    "zed"
    "flatpak"
    "zen"
    {
      matrix = {
        clients = [
          "element"
        ];
      };
    }
    "tuigreet"
    "writing"
  ];
in {
  flake.nixosModules.runar = lib'.mkNixosUser {
    inherit self;
    username = "runar";
    features = features.nixos;
    extraGroups = ["blahaj"];
  };

  flake.homeModules.runar = {
    pkgs,
    lib,
    ...
  }: {
    imports = features.home;

    programs.git.settings.user = {
      email = "git@runar.ch";
      name = "Runar Fredagsvik";
    };

    home.packages = with pkgs; [claude-code] ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [feishin me3 qbittorrent inputs.helium.packages.${pkgs.stdenv.hostPlatform.system}.default];

    home.stateVersion = "24.11";
  };

  flake.homeConfigurations.runar = lib'.mkUser {
    inherit self;
    username = "runar";
    homeModule = self.homeModules.runar;
  };
}
