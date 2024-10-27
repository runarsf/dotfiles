{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "mpv" {
  programs.mpv = {
    enable = true;

    # https://github.com/mpv-player/mpv/wiki/User-Scripts
    scripts = with pkgs; with mpvScripts; [
      uosc

      mpris
      webtorrent-mpv-hook
      videoclip
      thumbfast
      sponsorblock
      reload
      quality-menu
      mpv-slicing
      mpv-cheatsheet
      inhibit-gnome
      dynamic-crop
      youtube-upnext

      skip-intro
    ];
  };
}
