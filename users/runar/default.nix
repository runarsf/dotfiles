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
    imports = [
      { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; }

      # (inputs.nypkgs.lib.${system}.umport { path = ../../modules/users/development; })

      ../../modules/users/zsh.nix
      ../../modules/users/git.nix
      ../../modules/users/gpg.nix
      ../../modules/users/ssh.nix
      ../../modules/users/keychain.nix
      ../../modules/users/wallpaper.nix
      ../../modules/users/sops.nix
      ../../modules/users/zellij.nix

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

      imports = [
        ../../modules/users/discord.nix
        ../../modules/users/writing.nix
        ../../modules/users/xdg.nix
        ../../modules/users/spotify.nix
        ../../modules/users/development
        ../../modules/users/fonts.nix
        ../../modules/users/stylix.nix
        ../../modules/users/hyprland
        ../../modules/users/firefox
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
      # FIXME Module doesn't exist
      modules.android.enable = true;
      # modules.dev.android.enable = false;
      nixos.hardware.logitech.wireless.enable = true;

      home.packages =
        with pkgs.unstable;
        ifIsDesktop [
          solaar
          prismlauncher
          stremio
          obs-studio
          chromium
        ];
    };
  };
}
