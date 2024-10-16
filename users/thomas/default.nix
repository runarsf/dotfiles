{ config, inputs, outputs, system, hostname, name, pkgs, ... }:

outputs.lib.mkFor system hostname {
  common = {
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
      "nix"
      "yazi"
    ] // {
      wallpaper = ./wallpaper.jpg;
      sops = {
        enable = true;
        privateKeys = [ "id_priv" "id_ntnu" ];
      };
      ssh = {
        enable = true;
        publicKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBYghkkwi+HG+q91Xhcdc+Ac8wYdIo8BzUZKUPa2/00f thomes@stud.ntnu.no"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8glmBsdfxRsQxzZrljQynBF09jljQD4KIH33Kcx9Hw thoesp@protonmail.com"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFw8lBpuv2bWKYxxXeeG6pZ7Ut2GCtjuEbuvVEp9DmeY nix"
        ];
      };
    };

    programs.git = {
      userName = "Thomas Espervik";
      userEmail = "thoesp@protonmail.com";
    };
  };

  systems = {
    linux = {
      modules = outputs.lib.enable [
        "firefox"
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
        "bluetooth"
        "wezterm"
        "zen"
        "japanese"
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
          description = "Thomas";
          home = "/home/thomas";
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
    toaster = {
      isDesktop = true;

      modules = outputs.lib.enable [
        "ctf"
        "android"
        "android-ide"
        "steam"
        "ffxiv"
        "fun"
      ];
    };
  };
}
