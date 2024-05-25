{ pkgs, ... }:

# TODO Provide this as a parameter to the file
let lock = "${pkgs.hyprlock}/bin/hyprlock";

in {
  home.packages = with pkgs.unstable; [ hypridle ];

  wayland.windowManager.hyprland.settings.exec-once = [
    "${pkgs.hypridle}/bin/hypridle"
  ];

  xdg.configFile."hypr/hypridle.conf".text = ''
    general {
      lock_cmd = ${lock}
      before_sleep_cmd = ${lock}
    }
    listener {
      timeout = 300
      on-timeout = ${./. + /bin/hypr-brightness} off
      on-resume = ${./. + /bin/hypr-brightness} on
    }
    listener {
      timeout = 500
      on-timeout = ${lock}
    }
    listener {
      timeout = 900
      on-timeout = systemctl suspend
    }
  '';
}
