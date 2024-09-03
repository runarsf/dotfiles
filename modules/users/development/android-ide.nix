{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "android-ide" {
  home.packages = with pkgs;
    [
      (if (outputs.lib.isWayland config) then
        (builtins.trace
          "For Wayland support in Android Studio, add '-Dawt.toolkit.name=WLToolkit' to Help > Edit Custom VM Options..."
          unstable.android-studio)
      else
        unstable.android-studio)
    ];
}
