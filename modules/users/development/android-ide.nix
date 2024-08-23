{
  config,
  pkgs,
  outputs,
  ...
}:

outputs.lib.mkDesktopModule config "android-ide" {
  home.packages = with pkgs; [ unstable.android-studio ];
}
