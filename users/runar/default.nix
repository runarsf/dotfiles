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

    privateKeys = [
      "id_priv"
      "id_ntnu"
    ];
    publicKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z nix"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPcGHHx0Mx+zKzfq1eBig7CdNKKFdyv8W8AAFnHtDXx0 i@runar.ch"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINOTdUX/fNducOTIVd+y10N4gXjp5QxBAtBZAWqzH8ly runarsfr@stud.ntnu.no"
    ];

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
      "yazi"
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
        "sops-fonts"
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
