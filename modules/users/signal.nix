{ outputs, config, pkgs, ... }:

{
  home.packages = with pkgs.master;
    if (outputs.lib.isWayland config) then [ signal-wayland ] else [ signal-x ];

  nixpkgs.overlays = [
    (_: prev: {
      signal-wayland = prev.symlinkJoin {
        name = "signal-desktop";
        paths = [
          (prev.writeShellScriptBin "signal" ''
            exec ${pkgs.unstable.signal-desktop}/bin/signal-desktop \
              --use-tray-icon \
              --enable-features=UseOzonePlatform \
              --ozone-platform=wayland \
              $@
          '')
          prev.signal-desktop
        ];
      };
      signal-x = prev.symlinkJoin {
        name = "signal-desktop";
        paths = [
          (prev.writeShellScriptBin "signal" ''
            exec ${pkgs.unstable.signal-desktop}/bin/signal-desktop \
              --use-tray-icon \
              $@
          '')
          prev.signal-desktop
        ];
      };
    })
  ];
}
