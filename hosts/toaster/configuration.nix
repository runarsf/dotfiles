_: {
  imports = [
    ../../modules/linux/locales/norwegian.nix
    ../../modules/linux/systemd-boot.nix
    ../../modules/linux/thunderbolt.nix
    ../../modules/linux/network.nix
    ../../modules/linux/firewall.nix
    ../../modules/linux/printing.nix
  ];

  services.libinput.enable = true;
  services.xserver.enable = true;
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  system.stateVersion = "24.05";
}
