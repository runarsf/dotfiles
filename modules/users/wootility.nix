{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "wootility" {
  home.packages = with pkgs; [ wootility ];
  modules.udev.extraRules = [ ./wootility.rules ];
}
