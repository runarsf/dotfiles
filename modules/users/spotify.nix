{ config, outputs, pkgs, ... }:

outputs.lib.mkDesktopModule config "spotify" {
  home.packages = with pkgs; [
    (if (outputs.lib.isWayland config) then spotify-wayland else unstable.spotify)
  ];

  nixpkgs.overlays = [
    (_: prev: {
      spotify-wayland = prev.symlinkJoin {
        name = "spotify";
        paths = [
          (prev.writeShellScriptBin "spotify" ''
            exec ${pkgs.unstable.spotify}/bin/spotify \
              --enable-features=UseOzonePlatform,WaylandWindowDecorations \
              --ozone-platform-hint=auto \
              --ozone-platform=wayland \
              "$@"
          '')
          prev.spotify
        ];
      };
    })
  ];
}
