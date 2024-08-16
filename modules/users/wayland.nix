{ pkgs, ... }:

let waylandEnv = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
    LIBSEAT_BACKEND = "logind";
    XDG_SESSION_TYPE = "wayland";
    WLR_NO_HARDWARE_CURSORS = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    GDK_SCALE = "2";
    ELECTRON_OZONE_PLATFORM_HINT = "wayland";
  };

in {
  home.sessionVariables = waylandEnv;
  # NOTE This will break stuff if there is a non-wayland user on the same machine,
  #  but application launchers need this.
  # nixos.environment.sessionVariables = waylandEnv;

  nixos.services.libinput.enable = true;

  home.packages = with pkgs; [ xorg.xeyes ];

  # FIXME "Existing file '/home/runar/.config/mimeapps.list' is in the way ..."
  #  https://discourse.nixos.org/t/home-manager-and-the-mimeapps-list-file-on-plasma-kde-desktops/37694/4
  # xdg = {
  #   mimeApps.enable = true;
  #   portal = {
  #     enable = true;
  #     extraPortals = with pkgs; [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk ];
  #   };
  # };
}
