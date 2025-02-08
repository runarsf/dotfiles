{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "pyprland" {
  xdg.configFile."hypr/pyprland.json".text = builtins.toJSON {
    pyprland.plugins = [ "scratchpads" ];
    scratchpads = {
      term = {
        command = config.modules.${config.defaultTerminal}.exec {
          class = "scratchpad";
          # command = [ "connect" "scratchpad" ];
        };
        lazy = true;
        hide = false;
      };
      math = {
        command = config.modules.${config.defaultTerminal}.exec {
          class = "math-scratchpad";
          command = [ (outputs.lib.getExe pkgs.xonsh) ];
        };
        lazy = true;
        hide = false;
      };
    };
  };

  wayland.windowManager.hyprland.settings = {
    exec-once = [ "${pkgs.pyprland}/bin/pypr" ];
    bind = [
      "SUPER, N, exec, ${pkgs.pyprland}/bin/pypr toggle term"
      "SUPER, P, exec, ${pkgs.pyprland}/bin/pypr toggle math"
    ];
  };
}
