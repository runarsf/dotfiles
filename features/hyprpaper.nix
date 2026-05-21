{ self, inputs, ... }: {
  flake.homeModules.hyprpaper = { config, lib, ... }: {
    options.feature.hyprpaper.wallpaper = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to the wallpaper file.";
    };

    config = lib.mkIf (config.feature.hyprpaper.wallpaper != null) {
      services.hyprpaper = {
        enable = true;
        settings = {
          splash = false;
          wallpaper = [
            {
              monitor = "";
              path = "${config.feature.hyprpaper.wallpaper}";
              fit_mode = "cover";
            }
          ];
        };
      };
    };
  };
}
