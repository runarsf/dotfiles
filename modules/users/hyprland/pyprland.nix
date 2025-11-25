{
  config,
  pkgs,
  outputs,
  hypr-scratch-group,
  ...
}:
outputs.lib.mkDesktopModule config "pyprland" {
  home.packages = with pkgs; [
    octaveFull
  ];

  xdg.configFile."hypr/pyprland.json".text = builtins.toJSON {
    pyprland.plugins = [
      "toggle_special"
      "magnify"
    ];
    # pyprland.plugins = ["scratchpads"];
    # scratchpads = {
    #   # TODO uwsm start --
    #   term = {
    command = config.modules.terminal.exec' {
      class = "scratchpad";
      command = ["${outputs.lib.getExe pkgs.nushell}"];
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
    exec-once = [
      "${outputs.lib.getExe pkgs.pyprland}"
    ];
    bind = [
      # "SUPER, N, exec, ${pypr} toggle term"
      # "SUPER, P, exec, ${pypr} toggle math"
      "SUPER, Z, exec, ${pypr} zoom ++0.5"
      "SUPER SHIFT, Z, exec, ${pypr} zoom"

      "SUPER, N, togglespecialworkspace, scratchpad"
      "SUPER SHIFT, N, exec, ${pypr} toggle_special scratchpad"
    ];
    workspace = [
      # ''special:scratchpad, on-created-empty:${hypr-scratch-group} "wezterm start --class=scratch --" "scratch" "wezterm start -- octave-cli"''
      "special:scratchpad, on-created-empty:[size 1310 836] uwsm app -- ${
        config.modules.terminal.exec []
      }"
      # "special:scratchpad, on-created-empty:[size 22 70; move 75 70] uwsm app -- ${
      #   config.modules.${config.defaultTerminal}.exec {
      #     command = ["start" "--" "${outputs.lib.getExe' pkgs.octave "octave"}"];
      #   }
      # }"
      # "special:scratchpad, gapsout:50"
    ];
    windowrule = [
      "float on, match:workspace name:special:scratchpad"
    ];
  };
}
