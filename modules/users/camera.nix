{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "camera" {
  home.packages = with pkgs; [ cameractrls ];

  wayland.windowManager.hyprland.settings.exec-once =
    [ "${pkgs.cameractrls}/bin/cameractrlsd" ];
}
