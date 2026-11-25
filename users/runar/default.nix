{ inputs, ... }:
{
  users.runar = {
    root = ./.;

    stateVersion = "24.11";
    extraGroups = [ "blahaj" ];

    modules = [
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
          wallpaper = ./_config/wallpapers/nomai_chart.png;
          cursor = "wii";
        };
        dux = {
          aiUsage = true;
        };
        git = {
          name = "Runar Fredagsvik";
          email = "git@runar.ch";
        };
        hyprland = {
          animationPreset = "diablo";
        };
        wezterm = {
          bitmapFont = true;
        };
        ghostty = {
          bitmapFont = true;
        };
        terminal = {
          default = "ghostty";
        };
        artcraft = {
          photocraft.enable = true;
        };
      }
      "quantum-moon"
      "zsh"
      "matrix"
      "nautilus"
      "zellij"
      "wayland"
      "nushell"
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
          enable = false;
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
  };
}
