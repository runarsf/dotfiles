_: {
  flake.homeModules.hypridle = {
    lib,
    pkgs,
    osConfig,
    ...
  }: let
    hypr-brightness = pkgs.writeShellApplication {
      name = "hypr-brightness";
      runtimeInputs = [pkgs.brightnessctl];
      text = ''
        case "$1" in
          off)
            brightnessctl -s -q
            current=$(brightnessctl g)
            while [ "$current" -gt 1 ]; do
              current=$((current - 1200))
              brightnessctl s -q "$current"
              sleep 0.01
            done
            ;;
          on)
            brightnessctl -r -q
            ;;
        esac
      '';
    };
    lockCmd = lib.getExe pkgs.hyprlock;
  in {
    config = lib.mkIf (osConfig.host.desktop or true) {
      services.hypridle = {
        enable = true;
        settings = {
          general = {
            lock_cmd = lockCmd;
            before_sleep_cmd = lockCmd;
            after_sleep_cmd = "hyprctl dispatch \"hl.dsp.dpms({ action = \\\"on\\\" })\"";
            ignore_dbus_inhibit = false;
          };
          listener = [
            {
              timeout = 300;
              on-timeout = "${lib.getExe hypr-brightness} off";
              on-resume = "${lib.getExe hypr-brightness} on";
            }
            {
              timeout = 900;
              on-timeout = lockCmd;
            }
            {
              timeout = 1000;
              on-timeout = "hyprctl dispatch \"hl.dsp.dpms({ action = \\\"off\\\" })\"";
              on-resume = "hyprctl dispatch dpms on";
            }
          ];
        };
      };
    };
  };
}
