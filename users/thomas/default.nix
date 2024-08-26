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

    modules = outputs.lib.enable [
      "neovim"
      "zsh"
      "git"
      "gpg"
      "ssh"
      "keychain"
      "sops"
      "nix"
    ];

    wallpaper = ./wallpaper.jpg;

    programs.git = {
      userName = "Thomas Espervik";
      userEmail = "thoesp@protonmail.com";
    };
  };

  systems = {
    linux = {
      modules = outputs.lib.enable [
        "firefox"
        "discord"
        "fonts"
        "stylix"
        "vscode"
        "hyprland"
        "kitty"
        "python"
        "c"
        "c-ide"
        "writing"
        "spotify"
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

      modules = outputs.lib.enable [ "ctf" "steam" "ffxiv" ];
    };
  };
}
