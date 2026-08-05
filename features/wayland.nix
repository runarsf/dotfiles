{ ... }: {
  flake.homeModules.wayland = { pkgs, ... }: {
    home.packages = [ pkgs.wl-clipboard ];
    home.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      MOZ_ENABLE_WAYLAND = "1";
    };
  };
}
