{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "bluetooth" {
  services.blueman-applet.enable = true;

  nixos = {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
    services.blueman.enable = true;
    environment.systemPackages = with pkgs; [blueberry];
  };
}
