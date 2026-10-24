_: {
  homeModule = _: {
    wayland.windowManager.hyprland.settings = {
      monitor = [
        {
          output = "DP-1";
          mode = "2560x1440@144.0";
          position = "0x0";
          scale = 1;
          vrr = 0;
        }
        {
          output = "HDMI-A-1";
          mode = "1920x1080@60.0";
          position = "2560x300";
          scale = 1;
          vrr = 0;
        }
      ];
      workspace_rule = [
        {
          workspace = "1";
          monitor = "HDMI-A-1";
        }
        {
          workspace = "2";
          monitor = "HDMI-A-1";
        }
        {
          workspace = "4";
          monitor = "DP-1";
        }
        {
          workspace = "10";
          monitor = "DP-1";
        }
      ];
    };
  };
}
