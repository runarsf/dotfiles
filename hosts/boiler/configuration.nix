{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.boilerConfig = _: {
    imports = with self.linuxModuleConfigs; [
      norwegian
      systemd-boot
      thunderbolt
      network
      firewall
      printing
    ];

    services.libinput.enable = true;
    services.xserver.enable = true;
    services.displayManager.gdm.enable = true;
    services.desktopManager.gnome.enable = true;

    system.stateVersion = "24.05";

    boot.kernelParams = [
      "modset=1"
      # "i915.modeset=1"                                #
      # "nvidia_drm.modeset=1"                          # These 3 fuck up performance on GNOME for some reason...
      # "nvidia.NVreg_PreserveVideoMemoryAllocations=1" #
      "fbdev=1"
    ];
  };
}
