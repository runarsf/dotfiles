_: {
  homeModule = {
    thomas = _: {
      wayland.windowManager.hyprland.settings = {
        monitor = [
          {
            # Toaster screen
            output = "eDP-1";
            mode = "1920x1080@60.00100";
            position = "1920x0";
            scale = 1;
            vrr = 0;
          }
          {
            # Portable secondary monitor
            output = "DP-2";
            mode = "1920x1080@60.00000";
            position = "0x0";
            scale = 1;
            vrr = 0;
          }
        ];

        workspace_rule = [
          {
            workspace = "1";
            monitor = "eDP-1";
          }
          {
            workspace = "2";
            monitor = "eDP-1";
          }
          {
            workspace = "4";
            monitor = "DP-2";
          }
          {
            workspace = "10";
            monitor = "DP-2";
          }
        ];
      };
    };
  };
}
