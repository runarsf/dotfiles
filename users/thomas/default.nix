{
  config,
  inputs,
  outputs,
  system,
  hostname,
  name,
  pkgs,
  ...
}:

outputs.lib.mkFor system hostname {
  common = {
    imports = [
      { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; }

      ../../modules/users/development/neovim.nix
      ../../modules/users/development/nix.nix
      ../../modules/users/git.nix
      ../../modules/users/zsh.nix
      ../../modules/users/keychain.nix
      ../../modules/users/wallpaper.nix
      ../../modules/users/sops.nix
      ./config/secrets.nix
    ];

    modules.neovim.enable = true;
    modules.zsh.enable = true;
    modules.git.enable = true;
    modules.keychain.enable = true;
    modules.sops.enable = true;
    wallpaper = ./wallpaper.jpg;

    programs.git = {
      userName = "Thomas Espervik";
      userEmail = "thoesp@protonmail.com";
    };
  };

  systems = {
    linux = {
      imports = [
        ../../modules/users/discord.nix
        ../../modules/users/fonts.nix
        ../../modules/users/stylix.nix
        ../../modules/users/hyprland
        ../../modules/users/firefox
        ../../modules/users/development
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
    };
  };
}
