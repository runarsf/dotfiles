{ lib', ... }: {
  flake.homeModules.wayland = lib'.mkFeature "wayland" (
    { pkgs, lib, osConfig, ... }: {
      config = lib.mkIf (osConfig.host.desktop or true) {
        home.packages = [ pkgs.wl-clipboard ];
        home.sessionVariables = {
          NIXOS_OZONE_WL = "1";
          MOZ_ENABLE_WAYLAND = "1";
        };
      };
    }
  );
}
