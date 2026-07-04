_: {
  flake.homeModules.mpv = {pkgs, ...}: {
    programs.mpv = {
      enable = true;

      # https://github.com/mpv-player/mpv/wiki/User-Scripts
      scripts = with pkgs;
      with mpvScripts; [
        uosc

        mpris
        webtorrent-mpv-hook
        videoclip
        thumbfast
        sponsorblock
        reload
        quality-menu
        mpv-slicing
        mpv-cheatsheet-ng
        inhibit-gnome
        dynamic-crop
        youtube-upnext

        smart-skip
        smart-copy-paste
      ];
    };
  };
}
