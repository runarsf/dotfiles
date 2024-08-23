{
  config,
  pkgs,
  outputs,
  ...
}:

# TODO Move this into python.nix (does not work with deepMerge)
outputs.lib.mkDesktopModule config "python-ide" {
  home.packages = with pkgs; [ unstable.jetbrains.pycharm-professional ];
}
