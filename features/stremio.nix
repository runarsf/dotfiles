_: {
  flake.homeModules.stremio = {
    config,
    pkgs,
    lib,
    ...
  }: {
    home.packages = with pkgs; [
      (
        stremio.overrideAttrs (oldAttrs: {
          postInstall =
            oldAttrs.postInstall
            + ''
              sed -i 's|/usr/bin/mpv|${
                lib.getExe config.programs.mpv.finalPackage
              }|g' $out/opt/stremio/server.js
            '';
        })
      )
    ];
  };
}
