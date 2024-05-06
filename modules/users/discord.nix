{ outputs, config, pkgs, ... }:

{
  # Krisp: https://github.com/NixOS/nixpkgs/issues/195512

  home.packages = with pkgs.master;
    if (outputs.lib.isWayland config) then [ vesktop webcord ] else [ discord ];

  nixpkgs.overlays = [
    (_: prev: {
      discord = prev.discord.override {
        withOpenASAR = true;
        withVencord = true;
      };
      # vesktop = prev.symlinkJoin {
      #   name = "vencorddesktop";
      #   paths = [
      #     (prev.writeShellScriptBin "asd" ''
      #       exec ${pkgs.unstable.vesktop}/bin/vencorddesktop \
      #         --enable-features=UseOzonePlatform \
      #         --ozone-platform=wayland
      #         $@
      #     '')
      #     prev.vesktop
      #   ];
      # };
    })
  ];
}
