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

      ../../modules/users/development/neovim.nix
      ../../modules/users/development/nix.nix
      ../../modules/users/zsh.nix
      ../../modules/users/git.nix
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
    modules.keychain.enable = true;
    modules.sops.enable = true;
    modules.javascript.enable = true;
    wallpaper = ./wallpaper.jpg;

    programs.git = {
      userName = "Runar Fredagsvik";
      userEmail = "i@runar.ch";
    };
  };

  systems = {
    linux = {
      imports = [
        # ../../modules/users/easyeffects
        # ../../modules/users/gaming.nix
        # ../../modules/users/eww
        # ../../modules/users/writing.nix
        # ../../modules/users/mullvad.nix
        # ../../modules/users/xonsh.nix

        # TODO All modules should eventually have to be manually enabled, then you can just import ./.
        ../../modules/users/discord.nix
        # ../../modules/users/xdg.nix
        # ../../modules/users/spotify.nix
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
