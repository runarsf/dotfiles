{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "pyprland" {
  xdg.configFile."hypr/pyprland.json".text = builtins.toJSON {
    pyprland.plugins = ["toggle_special" "magnify"];
    # pyprland.plugins = ["scratchpads"];
    # scratchpads = {
    #   # TODO uwsm start --
    #   term = {
    command = config.modules.${config.defaultTerminal}.exec {
      class = "scratchpad";
      # command = [ "connect" "scratchpad" ];
    };
    #     lazy = true;
    #     hide = false;
    #   };
    #   math = {
    #     command = config.modules.${config.defaultTerminal}.exec {
    #       class = "math-scratchpad";
    #       command = [(outputs.lib.getExe pkgs.xonsh)];
    #     };
    #     lazy = true;
    #     hide = false;
    #   };
    # };
  };

  wayland.windowManager.hyprland.settings = let
    pypr = outputs.lib.getExe pkgs.pyprland;
  in {
    exec-once = ["${outputs.lib.getExe pkgs.pyprland}"];
    bind = [
      # "SUPER, N, exec, ${pypr} toggle term"
      # "SUPER, P, exec, ${pypr} toggle math"
      "SUPER, Z, exec, ${pypr} zoom ++0.5"
      "SUPER SHIFT, Z, exec, ${pypr} zoom"

      "SUPER, N, togglespecialworkspace, scratchpad"
      "SUPER SHIFT, N, exec, ${pypr} toggle_special scratchpad"
    ];
    workspace = [
      "special:scratchpad, on-created-empty:[size 1050 675] uwsm app -- ${
        config.modules.${config.defaultTerminal}.exec {
          # command = [ "connect" "scratchpad" ];
        }
      }"
      "special:scratchpad, gapsout:50"
    ];
    windowrulev2 = [
      "float, onworkspace:name:special:scratchpad"
    ];
  };
}
