{
  self,
  withSystem,
  lib',
  inputs,
  ...
}:
lib'.mkUser {
  inherit self withSystem;

  root = ./.;

  username = "runar";
  stateVersion = "24.11";
  extraGroups = [ "blahaj" ];

  features = [
    {
      sops = {
        privateKeys = [
          "id_priv"
          "id_golog"
        ];
      };
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
        hostKeys."gitlab.com" = "id_golog";
      };
      stylix = {
        wallpaper = ./outerwilds.jpeg;
        cursor = "wii";
      };
      dms = {
        aiUsage = true;
      };
      git = {
        name = "Runar Fredagsvik";
        email = "git@runar.ch";
      };
    }
    "matrix"
    "nautilus"
    "podman"
    "zellij"
    "wayland"
    "nushell"
    "hyprland"
    "claude"
    "zen"
    "writing"
  ];

  home =
    {
      pkgs,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system isLinux;
    in
    {
      home.packages =
        with pkgs;
        lib.optionals isLinux [
          feishin
          me3
          qbittorrent
          inputs.helium.packages.${system}.default
        ];

      programs.herdr = {
        enable = true;
        settings = {
          keys.prefix = "ctrl+b";
          onboarding = false;
          theme = {
            auto_switch = false;
            name = "vesper";
          };
          ui.status_indicators = "symbols";
        };
      };
    };
}
