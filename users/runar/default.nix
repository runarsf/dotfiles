{
  self,
  lib',
  inputs,
  ...
}: let
  features = lib'.useFeatures self [
    "sops"
    {
      ssh = {
        keys = [
          {
            name = "id_priv";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBT5zQFdVRooe5SfFZ2gKpruHF7FTw1OycTczRrLsR+M i@runar.ch";
          }
          {
            name = "id_nix";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z nix";
          }
          {
            name = "id_ntnu";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO6Y4kk5hFzs/B6vze9u9RPG9d+vVM5EIRIOug4OnJBk runarsfr@stud.ntnu.no";
          }
        ];
      };
    }
    "git"
    "fonts"
    "zsh"
    "flatpak"
    "starship"
    "wayland"
    "nixvim"
    "niri"
    "nushell"
    {
      hyprland = {
        nvidia = true;
      };
    }
    "hypridle"
    "hyprlock"
    "hyprpaper"
    "lumux"
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
    "zen"
    "orion"
    {
      matrix = {
        clients = [
          "element"
          "cinny"
        ];
      };
    }
    "tuigreet"
    "writing"
  ];
in {
  flake.nixosModules.runar = {
    pkgs,
    config,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    imports = features.nixos ++ [self.nixosModules.primaryUser];
    nix.settings.trusted-users = ["runar"];
    home-manager.users.runar = self.homeModules.runar;
    users.users.runar = {
      openssh.authorizedKeys.keys = map (k: k.key) config.features.ssh.keys;
      isNormalUser = true;
      shell = self.packages.${system}.zsh;
      home = "/home/runar";
      initialPassword = "changeme";
      description = "Runar Fredagsvik";
      extraGroups = [
        "wheel"
        "networkmanager"
        "docker"
        "audio"
        "video"
        "libvirtd"
        "input"
        "i2c"
        "blahaj"
      ];
    };
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

    home.packages = with pkgs; [claude-code] ++ lib.optionals pkgs.stdenv.isLinux [feishin me3 qbittorrent inputs.helium.packages.${pkgs.stdenv.hostPlatform.system}.default];

    home.stateVersion = "24.11";
  };

  flake.homeConfigurations.runar = lib'.mkUser {
    inherit self;
    username = "runar";
    homeModule = self.homeModules.runar;
  };
}
