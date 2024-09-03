{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "python-ide" {
  home.packages = with pkgs;
    [
      (if (outputs.lib.isWayland config) then
        (builtins.trace
          "For Wayland support in PyCharm, add '-Dawt.toolkit.name=WLToolkit' to Help > Edit Custom VM Options..."
          unstable.jetbrains.pycharm-professional)
      else
        unstable.jetbrains.pycharm-professional)
    ];
}
