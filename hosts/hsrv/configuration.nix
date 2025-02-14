_: {
  boot.initrd.luks.devices."nixos" = {
    device = "/dev/disk/by-uuid/a2c2165a-158b-4ef2-8d74-c7544b3883c7";
    preLVM = true;
  };

  programs.fuse.userAllowOther = true;
  system.stateVersion = "22.05";
}
