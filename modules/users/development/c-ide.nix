{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "c-ide" {
  home.packages = with pkgs;
    [
      (if (outputs.lib.isWayland config) then
        (builtins.trace
          "For Wayland support in CLion, add '-Dawt.toolkit.name=WLToolkit' to Help > Edit Custom VM Options..."
          unstable.jetbrains.clion)
      else
        unstable.jetbrains.clion)
    ];
}
