{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "logitech" {
  nixos = {
    hardware.logitech.wireless.enable = true;
    services.ratbagd.enable = true;
  };

  home.packages = with pkgs; [solaar piper];
}
