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
    imports = outputs.lib.umport { path = ../../modules/users; } ++ [
      { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; }
      ./config/secrets.nix
    ];

    modules.neovim.enable = true;
    modules.zsh.enable = true;
    modules.git.enable = true;
    modules.gpg.enable = true;
    modules.ssh.enable = true;
    modules.keychain.enable = true;
    modules.sops.enable = true;
    modules.nix.enable = true;

    wallpaper = ./wallpaper.jpg;

    programs.git = {
      userName = "Thomas Espervik";
      userEmail = "thoesp@protonmail.com";
    };
  };

  systems = {
    linux = {
      modules.firefox.enable = true;
      modules.discord.enable = true;
      modules.fonts.enable = true;
      modules.stylix.enable = true;
      modules.vscode.enable = true;
      modules.hyprland.enable = true;
      modules.kitty.enable = true;
      modules.python.enable = true;
      modules.c.enable = true;
      modules.c-ide.enable = true;
      modules.writing.enable = true;
      modules.spotify.enable = true;

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
