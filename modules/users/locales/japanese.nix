{ config, lib, outputs, pkgs, ... }:

# Intended to be used alongside another keyboard layout

outputs.lib.mkDesktopModule config "japanese" {
  wayland.windowManager.hyprland.settings.exec-once = [ "/run/current-system/systemd/bin/systemctl --user start xdg-autostart-if-no-desktop-manager.target" ];
  i18n.inputMethod.enabled = "fcitx5";
  gtk = {
    gtk2.extraConfig = ''gtk-im-module="fcitx"'';
    gtk3.extraConfig = { gtk-im-module = "fcitx"; };
    gtk4.extraConfig = { gtk-im-module = "fcitx"; };
  };
  dconf.settings."org/gnome/settings-daemon/plugins/xsettings" = {
    overrides = "{'Gtk/IMModule':<'fcitx'>}";
  };
  nixos = {
    # environment.systemPackages = with pkgs; [ fcitx5-configtool ];
    services.xserver.desktopManager.runXdgAutostartIfNone = true;

    # services.fcitx5 = {
    #   enable = true;
    #   defaultInputMethod = "mozc";
    #   globalHotkey = "TriggerKey=Ctrl+Space";
    # };

    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        ignoreUserConfig = true;
        addons = with pkgs; [ fcitx5-mozc ];
        settings.inputMethod = {
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
        waylandFrontend = outputs.lib.isWayland config;
      };
    };

    environment.sessionVariables = { XMODIFIERS = "@im=fcitx"; };
  };
}
