{
  config,
  outputs,
  ...
}: let
  inherit (outputs.lib) run runOnce getExe;
in
  outputs.lib.mkDesktopModule config "fuzzel" {
    programs.fuzzel = {
      enable = true;
      settings = {
        main = {
          # font = outputs.lib.mkForce "CaskaydiaCove NF:weight=normal:size=16";
          prompt = "' »  '";
          show-actions = true;
          list-executables-in-path = true;
          lines = 10;
          width = 45;
          tabs = 2;
          horizontal-pad = 5;
          vertical-pad = 5;
          inner-pad = 5;
          image-size-ratio = 0.25;
          dpi-aware = "no";
        };
        border = {
          width = 0;
          radius = 6;
        };
      };
    };

    wayland.windowManager.hyprland.settings.bind = [
      "SUPER, D, exec, ${runOnce ''${getExe config.programs.fuzzel.package} --launch-prefix="${run ""}"''}"
    ];
  }
