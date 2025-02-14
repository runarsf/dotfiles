{
  config,
  pkgs,
  outputs,
  ...
}: let
  lockCmd = "${pkgs.hyprlock}/bin/hyprlock";
in
  outputs.lib.mkDesktopModule config "hypridle" {
    services.hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = lockCmd;
          before_sleep_cmd = lockCmd;
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
        };

        listener = [
          {
            timeout = 300;
            on-timeout = "${./. + /bin/hypr-brightness} off";
            on-resume = "${./. + /bin/hypr-brightness} on";
          }
          {
            timeout = 900;
            on-timeout = lockCmd;
          }
          {
            timeout = 1000;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
          {
            timeout = 1200;
            on-timeout = "systemctl suspend";
          }
        ];
      };
    };

    # wayland.windowManager.hyprland.settings.exec-once = [ "${pkgs.hypridle}/bin/hypridle" ];
  }
