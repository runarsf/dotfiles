{
  config,
  pkgs,
  outputs,
  ...
}:

outputs.lib.mkDesktopModule config "c-ide" {
  home.packages = with pkgs; [ unstable.jetbrains.clion ];
}
