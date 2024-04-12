{ pkgs, ... }:

{
  home.packages = with pkgs; [ gnome.adwaita-icon-theme google-cursor ];

  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };
  nixos.programs.dconf.enable = true;

  qt = {
    enable = true;
    platformTheme = "gtk";
    # style.name = "adwaita-dark";
  };

  home.sessionVariables = {
    XCURSOR_THEME = "GoogleDot-Black";
    XCURSOR_SIZE = 24;
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
    cursorTheme = {
      name = "GoogleDot-Black";
      package = pkgs.google-cursor;
    };
    theme = {
      name = "Catppuccin-Mocha-Compact-Sapphire-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "sapphire" ];
        size = "compact";
        tweaks = [ "rimless" "black" ];
        variant = "mocha";
      };
    };
  };
}
