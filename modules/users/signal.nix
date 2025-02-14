{
  outputs,
  config,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "signal" {
  home.packages = with pkgs; [signal-desktop];
}
