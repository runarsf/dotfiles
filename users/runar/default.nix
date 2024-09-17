{ config, inputs, outputs, system, hostname, name, pkgs, ... }:

let ifIsDesktop = outputs.lib.optionals (outputs.lib.isDesktop config hostname);

in outputs.lib.mkFor system hostname {
  common = {
    # TODO Make this apply to all users
    # TODO Do this for hosts as well
    imports = outputs.lib.umport { path = ../../modules/users; }
      ++ outputs.lib.umport { path = ./config; } ++ [{
        _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ];
      }];

    privateKeys = [ "id_priv" "id_ntnu" ];
    publicKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z nix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBT5zQFdVRooe5SfFZ2gKpruHF7FTw1OycTczRrLsR+M i@runar.ch"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO6Y4kk5hFzs/B6vze9u9RPG9d+vVM5EIRIOug4OnJBk runarsfr@stud.ntnu.no"
    ];

    defaultTerminal = "wezterm";
    defaultBrowser = "zen";

    modules = outputs.lib.enable [
      "neovim"
      "zsh"
      "xonsh"
      "git"
      "gpg"
      "ssh"
      "keychain"
      "sops"
      "javascript"
      "nix"
      "yazi"
      "fun"
    ];

    wallpaper = ./wallpaper.jpg;
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
        "wezterm"
        "bluetooth"
        "zen"
      ];

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

      modules = outputs.lib.enable [ "ctf" "android" "android-ide" ];

      home.packages = with pkgs.unstable;
        ifIsDesktop [
          stremio
          obs-studio
          chromium

          # TODO Use android module
          android-tools
          scrcpy
        ];
    };
  };
}
