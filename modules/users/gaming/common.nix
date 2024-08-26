{ outputs, config, ... }:

outputs.lib.mkDesktopModule config "gaming" {
  nixos.services.ratbagd.enable = true;
}
