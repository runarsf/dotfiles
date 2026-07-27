_: {
  flake.homeModules.hyprpaper = _: {
    # Wallpaper set by stylix
    services.hyprpaper = {
      enable = true;
      settings = {
        splash = false;
      };
    };
  };
}
