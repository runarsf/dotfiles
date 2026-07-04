_: {
  flake.homeModules.hyprpaper = {
    config,
    lib,
    ...
  }: {
    options.features.hyprpaper.wallpaper = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to the wallpaper file.";
    };

    config = lib.mkIf (config.features.hyprpaper.wallpaper != null) {
      services.hyprpaper = {
        enable = true;
        settings = {
          splash = false;
          wallpaper = [
            {
              monitor = "";
              path = "${config.features.hyprpaper.wallpaper}";
              fit_mode = "cover";
            }
          ];
        };
      };
    };
  };
}
