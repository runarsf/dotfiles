{ config, inputs, outputs, system, hostname, name, pkgs, ... }:

outputs.lib.mkFor system hostname {
  common = {
    imports = [
      ../../modules/users/nix.nix
      ../../modules/users/convenience.nix
      ../../modules/users/git.nix
      ../../modules/users/starship.nix
      ../../modules/users/zsh.nix
      ../../modules/users/keychain.nix
    ];

    programs.git = {
      userName = "Thomas Espervik";
      userEmail = "thoesp@protonmail.com";
    };
  };

  systems = {
    linux = {
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
    boiler = {
      imports = [
        # NOTE Add modules from toaster
        ../../modules/users/gaming.nix
      ];
    };
    toaster = {
      imports = [
        ../../modules/users/desktop.nix
        ../../modules/users/hyprland
        ../../modules/users/firefox
        ../../modules/users/vscode.nix
        ../../modules/users/development.nix
        ../../modules/users/fonts.nix
        ../../modules/users/writing.nix
        ../../modules/users/kitty
      ];
      home.packages = with pkgs.unstable; [
        jetbrains.clion
        jetbrains.pycharm-professional
      ];
    };
  };
}
