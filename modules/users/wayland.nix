{ pkgs, ... }:

{
  imports = [
    ./gtk.nix
  ];

  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
  };

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
