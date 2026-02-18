{
  config,
  lib,
  outputs,
  pkgs,
  ...
}:
# Intended to be used alongside another keyboard layout
outputs.lib.mkDesktopModule config "japanese" {
  wayland.windowManager.hyprland.settings.exec-once = [
    "/run/current-system/systemd/bin/systemctl --user start xdg-autostart-if-no-desktop-manager.target"
  ];
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
  gtk = {
    gtk2.extraConfig = ''gtk-im-module="fcitx"'';
    gtk3.extraConfig = {gtk-im-module = "fcitx";};
    gtk4.extraConfig = {gtk-im-module = "fcitx";};
  };
  dconf.settings."org/gnome/settings-daemon/plugins/xsettings" = {
    overrides = "{'Gtk/IMModule':<'fcitx'>}";
  };
  home.sessionVariables = { XMODIFIERS = "@im=fcitx"; };
}
