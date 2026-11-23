{lib', ...}: {
  homeModule = _: let
    inherit (lib'.hyprland) kb;
  in {
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
        {
          # Toaster screen
          output = "desc:Chimei Innolux Corporation 0x14F2";
          mode = "1920x1080@60.00100";
          position = "1920x0";
          scale = 1;
          vrr = 0;
        }
        {
          # Portable secondary monitor
          output = "desc:Acer Technologies PM161Q B 15380108B3X00";
          mode = "1920x1080@60.00000";
          position = "0x0";
          scale = 1;
          vrr = 0;
        }
      ];
      workspace_rule = [
        {
          workspace = "1";
          monitor = "desc:Chimei Innolux Corporation 0x14F2";
        }
        {
          workspace = "2";
          monitor = "desc:Chimei Innolux Corporation 0x14F2";
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
      bind = [
        (kb "" "Pause" "hl.dsp.global(\"discord:discord-toggle-mute\")" {})
      ];
    };
  };
}
