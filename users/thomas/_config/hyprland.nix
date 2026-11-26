{lib', ...}: {
  homeModule = _: let
    inherit (lib'.hyprland) kb;
  in {
    wayland.windowManager.hyprland.settings = {
      config.scrolling.column_width = 1.0;
      bind = [
        (kb "" "Pause" "hl.dsp.global(\"discord:discord-toggle-mute\")" {})
      ];
    };
  };
}
