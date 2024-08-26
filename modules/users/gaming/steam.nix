{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "steam" {
  home.packages = with pkgs; [ steam protontricks winetricks ];
}