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
        "python"
        "c"
        "c-ide"
        "writing"
        "spotify"
        "sops-fonts"
        "wezterm"
        "bluetooth"
        "zen"
      ] // {
        stylix = {
          enable = true;
          system-wide = false;
          theme = "ayu-dark";
        };
      };

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

      modules =
        outputs.lib.enable [ "ctf" "android" "android-ide" "java" "flatpak" ];

      nixos.services.flatpak.packages = [ "hu.irl.cameractrls" ];

      home.packages = with pkgs.unstable;
        ifIsDesktop [
          p7zip
          stremio
          guvcview
          obs-studio
          chromium
          zed-editor

          # TODO Use android module
          android-tools
          scrcpy
        ];
    };

    rpi = {
      isDesktop = false;

      modules = outputs.lib.enable [ "sops" "podman" "gonic" ] // {
        nginx = {
          enable = true;
          domains = [ "runar.ch" ];
          email = "i@runar.ch";
        };
      };
    };
  };
}
