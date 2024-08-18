{ config, outputs, ... }:

outputs.lib.mkDesktopModule config "redshift" {
  services.redshift = {
    enable = true;
    tray = true;
    provider = "geoclue2";
  };

  nixos.services.geoclue2.enable = true;
}
