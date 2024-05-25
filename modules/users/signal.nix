{ outputs, config, pkgs, ... }:

{
  home.packages = with pkgs.master;
    if (outputs.lib.isWayland config) then [ signal-wayland ] else [ signal-desktop ];

  nixpkgs.overlays = [
    (_: prev: {
      signal-wayland = prev.symlinkJoin {
        name = "signal-desktop";
        paths = [
          (prev.writeShellScriptBin "signal" ''
            exec ${pkgs.unstable.signal-desktop}/bin/signal-desktop \
              --use-tray-icon \
              --enable-features=UseOzonePlatform \
              --ozone-platform=wayland
              $@
          '')
          prev.signal-desktop
        ];
      };
    })
  ];
}
