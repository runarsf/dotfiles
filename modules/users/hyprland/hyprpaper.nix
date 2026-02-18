{
  config,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "hyprpaper" {
  services.hyprpaper = {
    enable = true;

    settings = {
      splash = false;
      wallpaper = [
        {
          monitor = "";
          path = "${config.modules.wallpaper}";
          fit_mode = "cover";
        }
      ];
    };
  };
}
