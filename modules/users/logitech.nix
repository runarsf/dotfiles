{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "logitech" {
  nixos.hardware.logitech.wireless.enable = true;

  home.packages = with pkgs; [ solaar ];
}
