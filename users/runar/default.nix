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
    # TODO Do this for hosts as well
    imports =
      outputs.lib.umport { path = ../../modules/users; }
      ++ outputs.lib.umport { path = ./config; }
      ++ [ { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; } ];

    modules = outputs.lib.enable [
      "zellij"
      "neovim"
      "zsh"
      "xonsh"
      "git"
      "gpg"
      "ssh"
      "keychain"
      "sops"
      "javascript"
      "nix"
    ];

    wallpaper = ./wallpaper.jpg;
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

      modules = outputs.lib.enable [ "ctf" ];

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
