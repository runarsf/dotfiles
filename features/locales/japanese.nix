_: {
  flake.nixosModules.norwegian = {pkgs, ...}: {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        settings = {
          inputMethod = {
            "Groups/0" = {
              "Name" = "Default";
              "Default Layout" = "no";
              "DefaultIM" = "mozc";
            };
            "Groups/0/Items/0" = {
              "Name" = "keyboard-no";
              "Layout" = null;
            };
            "Groups/0/Items/1" = {
              "Name" = "mozc";
              "Layout" = null;
            };
          };
        };
        addons = with pkgs; [qt6Packages.fcitx5-configtool fcitx5-mozc fcitx5-gtk];
        # waylandFrontend = true;
      };
    };
  };

  flake.homeModules.japanese = _: {
    home.sessionVariables = {XMODIFIERS = "@im=fcitx";};
  };
}
