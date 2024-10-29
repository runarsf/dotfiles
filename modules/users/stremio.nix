{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "stremio" {
  modules.mpv.enable = true;

  home.packages = with pkgs.unstable;
    [
      (stremio.overrideAttrs (oldAttrs: {
        postInstall = oldAttrs.postInstall + ''
          sed -i 's|/usr/bin/mpv|${
            outputs.lib.getExe config.programs.mpv.finalPackage
          }|g' $out/opt/stremio/server.js
        '';
      }))
    ];
}
