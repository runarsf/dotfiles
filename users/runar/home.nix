{ outputs, system, hostname, name, pkgs, ... }:

outputs.lib.mkFor system hostname {
  common = {
    imports = [
      ../../modules/users/convenience.nix
      ../../modules/users/development.nix
      ../../modules/users/fonts.nix
      ../../modules/users/git.nix
      ../../modules/users/kitty.nix
      ../../modules/users/neovim.nix
      ../../modules/users/starship.nix
      ../../modules/users/zsh.nix
      ../../modules/users/writing.nix
      ../../modules/users/tmux.nix
      ../../modules/users/lf

      # ../../modules/users/sops.nix
      # ./config/secrets.nix
    ];

    programs.git = {
      userName = "Runar Fredagsvik";
      userEmail = "i@runar.ch";
    };

    home = {
      sessionVariables = {
        EDITOR = "nvim";
        GIT_EDITOR = "nvim";
        VISUAL = "nvim";
        DIFFPROG = "nvim -d";
        MANPAGER = "nvim +Man!";
        MANWIDTH = 999;
      };
    };
  };

  systems = {
    linux = {
      imports = [
        ../../modules/users/discord.nix
        ../../modules/users/gtk.nix
        ../../modules/users/wallpaper.nix
        ../../modules/users/gaming.nix
        ../../modules/users/gtk.nix
        ../../modules/users/dunst.nix
        ../../modules/users/desktop.nix
        ../../modules/users/hyprland
      ];
      wallpaper = ./wallpaper.jpg;
      system = {
        programs.zsh.enable = true;
        users.users.${name} = {
          isNormalUser = true;
          initialPassword = "changeme";
          description = "Runar";
          home = "/home/runar";
          shell = pkgs.zsh;
          extraGroups = [ "wheel" "networkmanager" "docker" "audio" "video" "libvirtd" "input"];
        };
      };
    };
  };

  hosts = {
    # runix = {
    #   programs.git = {
    #     userName = "Runar Fredagsvik";
    #     userEmail = "i@runar.ch";
    #     signing = {
    #       key = "";
    #       signByDefault = true;
    #     };
    #   };
    # };
  };
}
