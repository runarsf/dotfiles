{ config, inputs, outputs, system, hostname, name, pkgs, ... }:

outputs.lib.mkFor system hostname {
  common = {
    imports = outputs.lib.umport { path = ../../modules/users; }
      ++ outputs.lib.umport { path = ./config; } ++ [{
        _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ];
      }];

    privateKeys = [ "id_priv" "id_ntnu" ];
    publicKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBYghkkwi+HG+q91Xhcdc+Ac8wYdIo8BzUZKUPa2/00f thomes@stud.ntnu.no"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8glmBsdfxRsQxzZrljQynBF09jljQD4KIH33Kcx9Hw thoesp@protonmail.com"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFw8lBpuv2bWKYxxXeeG6pZ7Ut2GCtjuEbuvVEp9DmeY nix"
    ];

    defaultTerminal = "wezterm";

    modules = outputs.lib.enable [
      "neovim"
      "zsh"
      "xonsh"
      "git"
      "gpg"
      "ssh"
      "keychain"
      "sops"
      "nix"
      "yazi"
    ];

    wallpaper = ./wallpaper.jpg;

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
        "stylix"
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
      ];

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

      modules = outputs.lib.enable [ "ctf" "steam" "ffxiv" ];
    };
  };
}
