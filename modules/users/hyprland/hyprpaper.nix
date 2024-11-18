{ config, outputs, ... }:

# TODO hyprpaper
outputs.lib.mkDesktopModule config "hyprpaper" {
  services.hyprpaper = {
    enable = true;
    settings = {
      ipc = "on";
      splash = false;
    };
  };
}
