{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "stremio" {
  home.packages = with pkgs; [
    (
      if config.modules.mpv.enable
      then
        stremio.overrideAttrs (oldAttrs: {
          postInstall =
            oldAttrs.postInstall
            + ''
              sed -i 's|/usr/bin/mpv|${
                outputs.lib.getExe config.programs.mpv.finalPackage
              }|g' $out/opt/stremio/server.js
            '';
        })
      else stremio
    )
  ];
}
