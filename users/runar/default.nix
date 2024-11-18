{ config, inputs, outputs, system, hostname, name, pkgs, ... }:

let
  ifIsDesktop = outputs.lib.optionals
    config.isDesktop; # (outputs.lib.isDesktop config hostname);

in outputs.lib.mkFor system hostname {
  common = {
    # TODO Make this apply to all users
    imports = outputs.lib.concatImports { path = ../../modules/users; }
      ++ outputs.lib.concatImports { path = ./config; } ++ [{
        _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ];
      }];

    defaultTerminal = "wezterm";
    defaultBrowser = "zen";
    avatar = ./avatar.jpg;

    modules = outputs.lib.enable [
      "neovim"
      "zsh"
      "xonsh"
      "git"
      "gpg"
      "keychain"
      "javascript"
      "nix"
      "yazi"
      "fun"
    ] // {
      wallpaper = ./wallpaper.jpg;
      sops = {
        enable = true;
        privateKeys = [ "id_priv" "id_ntnu" ];
      };
      ssh = {
        enable = true;
        publicKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z nix"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBT5zQFdVRooe5SfFZ2gKpruHF7FTw1OycTczRrLsR+M i@runar.ch"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO6Y4kk5hFzs/B6vze9u9RPG9d+vVM5EIRIOug4OnJBk runarsfr@stud.ntnu.no"
        ];
      };
    };
  };

  systems = {
    linux = {
      modules = outputs.lib.enable [
        "discord"
        "fonts"
        "vscode"
        "hyprland"
        "kitty"
        "c"
        "c-ide"
        "writing"
        "spotify"
        "sops-fonts"
        "wezterm"
        "bluetooth"
        "zen"
        "pipewire"
        "mpv"
        "stremio"
      ] // {
        stylix = {
          enable = true;
          system-wide = false;
          theme = "ayu-dark";
        };
        python = {
          enable = true;
          packageName = "python311";
          presets = outputs.lib.enable [ "math" "jupyter" ];
        };
        flatpak.enable = config.isDesktop;
      };

      home.packages = with pkgs.unstable;
        ifIsDesktop [ p7zip guvcview obs-studio chromium protonvpn-gui ];

      nixos.services.flatpak.packages = ifIsDesktop [ "hu.irl.cameractrls" ];

      nixos = {
        programs.zsh.enable = true;
        users.users."${name}" = {
          isNormalUser = true;
          initialPassword = "changeme";
          description = "Runar";
          home = "/home/runar";
          shell = pkgs.zsh;
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
    };
  };

  hosts = {
    runix = {
      isDesktop = true;

      modules = outputs.lib.enable [
        "ctf"
        "android"
        "android-ide"
        "java"
        "easyeffects"
        "audiorelay"
      ];

      home.packages = with pkgs.unstable; [ pokemmo-installer ];

      xdg.desktopEntries."steam-handler" = {
        type = "Application";
        name = "Steam Handler";
        mimeType = [ "x-scheme-handler/steam" ];
        exec = "${
            pkgs.writeShellScript "steam-handler" ''
              #!/run/current-system/sw/bin/bash
              set -o errexit
              set -o nounset

              notify-send "Steam trynna open $1"
            ''
          } %u";
      };
    };

    rpi = {
      isDesktop = false;

      modules = outputs.lib.enable [ "sops" "podman" ] // {
        services.gonic.enable = true;
        nginx = {
          enable = true;
          domains = [ "runar.ch" ];
          email = "ssl@runar.ch";
        };
      };
    };
  };
}
