{
  self,
  lib',
  inputs,
  ...
}:
let
  features = lib'.useFeatures self [
    {
      sops = {
        privateKeys = [
          "id_priv"
          "id_golog"
        ];
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
    {
      matrix = {
        clients = [
          "element"
        ];
      };
    }
    "tuigreet"
    "git"
    "podman"
    "discord"
    "fonts"
    "zsh"
    "zellij"
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
    "wezterm"
    "niks"
    "nix"
    "vicinae"
    "network"
    "cli"
    "norwegian"
    "pipewire"
    "zed"
    "claude"
    "flatpak"
    "zen"
    "writing"
  ];
in
{
  flake.nixosModules.runar = lib'.mkNixosUser {
    inherit self;
    username = "runar";
    features = features.nixos;
    extraGroups = [ "blahaj" ];
  };

  flake.homeModules.runar =
    {
      pkgs,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system isLinux;
    in
    {
      imports = features.home;

      programs.git.settings.user = {
        email = "git@runar.ch";
        name = "Runar Fredagsvik";
      };

        # programs.ssh.matchBlocks."gitlab.com" = {
        #   hostname = "altssh.gitlab.com";
        #   user = "git";
        #   port = 443;
        #   extraOptions.PreferredAuthentications = "publickey";
        #   # identityFile = "~/.ssh/id_priv.pub";
        # };

      home.packages =
        with pkgs;
        lib.optionals isLinux [
          feishin
          me3
          qbittorrent
          inputs.helium.packages.${system}.default
        ];

      home.stateVersion = "24.11";
    };

  flake.homeConfigurations.runar = lib'.mkUser {
    inherit self;
    username = "runar";
    homeModule = self.homeModules.runar;
  };
}
