{ config, outputs, system, hostname, name, pkgs, ... }:

outputs.lib.mkFor system hostname {
  common = {
    imports = [
      { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_priv" ]; }

      ../../modules/users/convenience.nix
      ../../modules/users/development.nix
      ../../modules/users/fonts.nix
      ../../modules/users/git.nix
      ../../modules/users/starship.nix
      ../../modules/users/zsh.nix
      ../../modules/users/writing.nix
      ../../modules/users/tmux.nix
      ../../modules/users/lf
      ../../modules/users/keychain.nix
      ../../modules/users/sops.nix
      ../../modules/users/kitty
      ../../modules/users/mullvad.nix
      ./config/secrets.nix
    ];

    programs.git = {
      userName = "Runar Fredagsvik";
      userEmail = "i@runar.ch";
    };
  };

  systems = {
    linux = {
      imports = [
        ../../modules/users/desktop.nix
        ../../modules/users/wallpaper.nix
        ../../modules/users/gaming.nix
        ../../modules/users/hyprland
        ../../modules/users/eww
        ../../modules/users/firefox
      ];
      home.packages = with pkgs; [ obs-studio ];
      wallpaper = ./wallpaper.jpg;
      system = {
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
          ];
        };
      };
    };
  };

  hosts = {
    runix = {
      imports = [
        #   (import ../../modules/users/red.nix { inherit inputs pkgs; })
        ../../modules/users/adb.nix
        # ../../modules/users/warp.nix
      ];
      system.virtualisation.waydroid.enable = true;
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
