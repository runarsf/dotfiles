{ config, pkgs, outputs, ... }:

# TODO Make VM options automatic: ~/.config/JetBrains/CLion2024.1/clion64.vmoptions
#  Maybe using xdg.configFile and onChange

outputs.lib.mkDesktopModule config "c-ide" {
  home.packages = with pkgs;
    [
      (if (outputs.lib.isWayland config) then
        (builtins.trace
          "For Wayland support in CLion, add '-Dawt.toolkit.name=WLToolkit' to [Help > Edit Custom VM Options...]"
          unstable.jetbrains.clion)
      else
        unstable.jetbrains.clion)
    ];
}
