{
  config,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "bluetooth" {
  services.blueman-applet.enable = true;

  nixos = {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = false;
    };
    services.blueman.enable = true;
  };
}
