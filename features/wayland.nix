{ ... }: {
  flake.homeModules.wayland = { pkgs, ... }: {
    home.packages = [ pkgs.wl-clipboard ];
  };
}
