{ pkgs, inputs, outputs, ... }:

{
  # NOTE Stylix requires both nixos and home-manager to have the same stateVersion
  # FIXME https://github.com/danth/stylix/issues/241
  imports = [
    inputs.stylix.homeManagerModules.stylix
  ];

  nixos.environment.systemPackages = with pkgs; [
    gtk2
    gtk3
    gtk4
  ];

  xdg.systemDirs.data = [
    "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
    "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}"
  ];

  gtk = {
    enable = true;

    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
      gtk-cursor-theme-name = "Qogir Cursors";
    };
  };

  stylix = {
    enable = true;
    polarity = "dark";

    # ~/.config/stylix/palette.html
    # /etc/stylix/palette.html
    # https://github.com/tinted-theming/schemes
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";

    targets = outputs.lib.disable [
      "hyprland"
      "nixvim"
      "kitty"
      "waybar"
      "vscode"
    ];

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 30;
    };

    image = ../../users/runar/wallpaper.jpg;

    opacity = {
      terminal = 0.8;
      popups = 0.9;
    };

    fonts = {
      sizes = {
        terminal = 14;
        applications = 12;
        desktop = 10;
        popups = 10;
      };

      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
      monospace = {
        package = pkgs.unstable.nerdfonts.override {
          fonts = [ "JetBrainsMono" ];
        };
        name = "JetBrainsMono Nerd Font";
      };
      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };
  };
}
