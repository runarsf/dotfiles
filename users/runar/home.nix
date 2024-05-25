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
        #   (import ../../modules/users/red.nix { inherit inputs pkgs; })
        ../../modules/users/android.nix
        ../../modules/users/desktop.nix
        # ../../modules/users/gaming.nix
        ../../modules/users/hyprland
        ../../modules/users/eww
        ../../modules/users/firefox
        ../../modules/users/vscode.nix
        ../../modules/users/development.nix
        ../../modules/users/fonts.nix # TODO isDesktop
        ../../modules/users/writing.nix # TODO isDesktop
        ../../modules/users/kitty # TODO isDesktop
        ../../modules/users/mullvad.nix # TODO isDesktop
        ../../modules/users/xonsh.nix
        ../../modules/users/gnome.nix
      ];
      home.packages = with pkgs.unstable; [
        obs-studio
        stremio
        chromium

        qmk

        jetbrains.clion
        jetbrains.pycharm-professional
      ];
      # nixos.services.udev.packages = with pkgs; [ via ];
        # for VIA
        # KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"
        nixos.services.udev.packages = let
          qmk-rules = pkgs.writeTextFile {
            name = "50-qmk.rules";
            text = builtins.readFile ./qmk.rules;
            destination = "/etc/udev/rules.d/50-qmk.rules";
          };
        in [ qmk-rules ];
      nixos.services.udev.extraRules = builtins.readFile ./qmk.rules;
      # nixos.services.udev.extraRules = ''
      #   # For ATMega32u4 keyboards like the viterbi
      #   # Atmel ATMega32U4
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff4", TAG+="uaccess"
      #   # Atmel USBKEY AT90USB1287
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ffb", TAG+="uaccess"
      #   # Atmel ATMega32U2
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff0", TAG+="uaccess"
      #   # Atmel ATMega328p / USBAspLoader
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="05dc", TAG+="uaccess"

      #   # For OLKB keyboards
      #   # stm32duino
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="1eaf", ATTRS{idProduct}=="0003", MODE:="0666"
      #   # Generic stm32
      #   SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", MODE:="0666"
      # '';
      # nixos.virtualisation.waydroid.enable = true;
      #   programs.git = {
      #     userName = "Runar Fredagsvik";
      #     userEmail = "i@runar.ch";
      #     signing = {
      #       key = "";
      #       signByDefault = true;
      #     };
      #   };
    };
  };
}
