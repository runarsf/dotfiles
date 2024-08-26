{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "ffxiv" {
  home.packages = with pkgs; [ xivlauncher ];
}