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
        ../../modules/users/adb.nix
        ../../modules/users/desktop.nix
        ../../modules/users/gaming.nix
        ../../modules/users/hyprland
        ../../modules/users/eww
        ../../modules/users/firefox
        ../../modules/users/vscode.nix
        ../../modules/users/development.nix
        ../../modules/users/fonts.nix # TODO isDesktop
        ../../modules/users/writing.nix # TODO isDesktop
        ../../modules/users/kitty # TODO isDesktop
        ../../modules/users/mullvad.nix # TODO isDesktop
      ];
      home.packages = with pkgs.unstable; [
        obs-studio

        jetbrains.clion
        jetbrains.pycharm-professional

        graphviz
        pkgs.master.warp-terminal
      ];
      nixos.virtualisation.waydroid.enable = true;
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
