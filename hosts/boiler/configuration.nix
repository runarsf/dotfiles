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
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  system.stateVersion = "24.05";

  boot.kernelParams = [
    "modset=1"
    # "i915.modeset=1"                                #
    # "nvidia_drm.modeset=1"                          # These 3 fuck up performance on GNOME for some reason...
    # "nvidia.NVreg_PreserveVideoMemoryAllocations=1" #
    "fbdev=1"
  ];
}
