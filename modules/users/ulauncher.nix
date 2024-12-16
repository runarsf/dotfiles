{ config, pkgs, outputs, ... }:

let
  options = builtins.toJSON {

  };

in outputs.lib.mkDesktopModule config "ulauncher" {
  wayland.windowManager.hyprland.settings = {
    # TODO Use ulauncher service
    exec = [
      ''
        pgrep -x ".ulauncher-wrap" || ${pkgs.ulauncher}/bin/ulauncher --no-window-shadow --hide-window''
    ];

    bind = [ "SUPER, D, exec, ${pkgs.ulauncher}/bin/ulauncher-toggle" ];
  };

  # extensions, settings, shortcuts
  home.file =
    outputs.lib.mkEditableHomeFile pkgs "~/.config/ulauncher/settings.json"
    (builtins.toJSON { hello = "wworld"; });
}
