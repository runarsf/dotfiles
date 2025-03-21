{inputs, ...}: {
  # TODO Make most of these modules user-based
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ../../modules/linux/locales/norwegian.nix
    ../../modules/linux/systemd-boot.nix
    ../../modules/linux/thunderbolt.nix
    ../../modules/linux/network.nix
    ../../modules/linux/firewall.nix
    ../../modules/linux/printing.nix
  ];

  # TODO Make this user config
  # services.openssh = {
  #   enable = true;
  #   settings.PermitRootLogin = "yes";
  # };

  services.libinput.enable = true;
  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  system.stateVersion = "24.05";
}
