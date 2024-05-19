_:

{
  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };
  nixos.services.xserver = {
    # displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
}
