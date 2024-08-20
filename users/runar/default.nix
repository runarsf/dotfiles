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

let
  ifIsDesktop = outputs.lib.optionals (outputs.lib.isDesktop config hostname);

in
outputs.lib.mkFor system hostname {
  common = {
    # TODO Make this apply to all users
    imports = outputs.lib.umport { path = ../../modules/users; } ++ [
      { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; }
      ./config/secrets.nix
    ];

    modules.zellij.enable = true;
    modules.neovim.enable = true;
    modules.zsh.enable = true;
    modules.git.enable = true;
    modules.gpg.enable = true;
    modules.ssh.enable = true;
    modules.keychain.enable = true;
    modules.sops.enable = true;
    modules.javascript.enable = true;
    modules.nix.enable = true;

    wallpaper = ./wallpaper.jpg;

    programs.git = {
      userName = "Runar Fredagsvik";
      userEmail = "i@runar.ch";
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
      modules.python-ide.enable = true;
      modules.c.enable = true;
      modules.c-ide.enable = true;
      modules.writing.enable = true;
      modules.spotify.enable = true;

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

      home.packages =
        with pkgs.unstable;
        ifIsDesktop [
          stremio
          obs-studio
          chromium
        ];
    };
  };
}
