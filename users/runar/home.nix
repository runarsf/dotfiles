{ config, inputs, outputs, system, hostname, name, pkgs, ... }:

outputs.lib.mkFor system hostname {
  common = {
    imports = [
      { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/nix" ]; }

      ../../modules/users/nix.nix
      ../../modules/users/convenience.nix
      ../../modules/users/git.nix
      ../../modules/users/starship.nix
      ../../modules/users/zsh.nix
      ../../modules/users/tmux.nix
      ../../modules/users/lf
      ../../modules/users/keychain.nix
      ../../modules/users/sops.nix
      ./config/secrets.nix
    ];

    programs.git = {
      userName = "Runar Fredagsvik";
      userEmail = "i@runar.ch";
    };
  };

  systems = {
    linux = {
      nixos = {
        programs.zsh.enable = true;
        users.users."${name}" = {
          isNormalUser = true;
          initialPassword = "changeme";
          # TODO https://github.com/Mic92/sops-nix#setting-a-users-password
          # hashedPasswordFile = config.sops.secrets.password_runar.path;
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
      # TODO isDesktop
      imports = [
        ../../modules/users/stylix.nix
        ../../modules/users/android.nix
        # ../../modules/users/easyeffects
        ../../modules/users/desktop.nix
        # ../../modules/users/gaming.nix
        ../../modules/users/hyprland
        # ../../modules/users/eww
        ../../modules/users/firefox
        ../../modules/users/vscode.nix
        ../../modules/users/development.nix
        ../../modules/users/fonts.nix # TODO isDesktop
        ../../modules/users/writing.nix # TODO isDesktop
        ../../modules/users/kitty # TODO isDesktop
        ../../modules/users/mullvad.nix # TODO isDesktop
        ../../modules/users/xonsh.nix
      ];
      home.packages = with pkgs.unstable; [
        obs-studio
        stremio
        chromium

        # TODO Make qmk module
        qmk

        jetbrains.clion
        jetbrains.pycharm-professional
        jetbrains.rider
      ];
      nixos.services.udev.packages =
        let
          qmk-rules = pkgs.writeTextFile {
            name = "50-qmk.rules";
            text = builtins.readFile ./qmk.rules;
            destination = "/etc/udev/rules.d/50-qmk.rules";
          };
        in [ qmk-rules ];
      nixos.services.udev.extraRules = builtins.readFile ./qmk.rules;
    };
  };
}
