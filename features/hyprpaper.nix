_: {
  flake.homeModules.hyprpaper = {
    lib,
    osConfig,
    ...
  }: {
    config = lib.mkIf (osConfig.host.desktop or true) {
      # Wallpaper set by stylix
      services.hyprpaper = {
        enable = true;
        settings = {
          splash = false;
        };
      };
    };
  };
}
