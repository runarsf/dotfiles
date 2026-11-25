{lib', ...}: {
  homeModule = {lib, ...}: let
    inherit (lib'.hyprland) kb;
    inherit (lib) flatten;

    primaries = [
      "DP-1" # desktop
      "DP-2" # laptop
    ];

    secondaries = [
      "HDMI-A-1" # desktop
      "eDP-1" # laptop
    ];

    primary_rules =
      map (monitor: [
        {
          workspace = "4";
          monitor = monitor;
        }
        {
          workspace = "10";
          monitor = monitor;
        }
      ])
      primaries;

    secondary_rules =
      map (monitor: [
        {
          workspace = "1";
          monitor = monitor;
        }
        {
          workspace = "2";
          monitor = monitor;
        }
      ])
      secondaries;
  in {
    wayland.windowManager.hyprland.settings = {
      config.scrolling.column_width = 1.0;
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
      workspace_rule = (primary_rules ++ secondary_rules) |> flatten;
      bind = [
        (kb "" "Pause" "hl.dsp.global(\"discord:discord-toggle-mute\")" {})
      ];
    };
  };
}
