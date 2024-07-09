{ outputs, config, pkgs, ... }:

{
  # FIXME Discord uses x?
  home.packages = with pkgs;
    if (outputs.lib.isWayland config) then [ waycord vesktop ] else [ discord ];

  nixpkgs.overlays = [
    (final: prev: {
      discord = prev.discord.override {
        withOpenASAR = true;
        withVencord = true;
      };
      waycord = prev.symlinkJoin {
        name = "discord";
        paths = [
          (prev.writeShellScriptBin "discord" ''
            exec ${final.discord}/bin/discord \
              --enable-features=UseOzonePlatform,WaylandWindowDecorations \
              --ozone-platform-hint=auto \
              --ozone-platform=wayland \
              "$@"
          '')
          final.discord
        ];
      };
    })
  ];
}
