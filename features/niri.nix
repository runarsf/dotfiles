{
  self,
  inputs,
  ...
}:
{
  # NOTE: Run `niri validate` to get path to config
  # focus-workspace-down / focus-workspace-up
  # switch-focus-between-floating-and-tiling
  # Mod+W { toggle-column-tabbed-display; }
  # tab-indicator.place-within-column
  flake.nixosModules.niri =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
    in
    {
      programs.niri = {
        enable = true;
        package = self.packages.${system}.niri;
      };

      environment.systemPackages = with pkgs; [
        nwg-displays
        inputs.niri-scratchpad.packages.${pkgs.stdenv.hostPlatform.system}.default
      ];
    };

  perSystem =
    {
      pkgs,
      lib,
      ...
    }:
    {
      # TODO: polkit and portal https://github.com/niri-wm/niri/wiki/Important-Software
      packages.niri = inputs.wrapper-modules.wrappers.niri.wrap {
        inherit pkgs;

        extraSettings = [
          {
            include = [
              { optional = true; }
              "~/.config/niri/monitor.kdl"
            ];
          }
        ];
        settings = {
          input = {
            keyboard = {
              xkb.layout = "no";
            };
            mouse = {
              accel-profile = "flat";
            };
            focus-follows-mouse = _: { };
            touchpad = {
              natural-scroll = _: { };
              tap = _: { };
            };
          };

          layout = {
            gaps = 10;

            background-color = "transparent";

            shadow = {
              on = _: { };
              softness = 30;
              spread = 5;
              offset = _: {
                props = {
                  x = 0;
                  y = 5;
                };
              };
              color = "#00000064";
            };
            focus-ring = {
              width = 2;
              active-gradient = _: {
                props = {
                  from = "#ff5c8a";
                  to = "#ff6a70";
                  angle = 45;
                  "in" = "oklch longer hue";
                };
              };
              inactive-color = "#2e2e2e";
            };
          };

          outputs =
            let
              vrr = {
                "variable-refresh-rate" = _: {
                  props = {
                    "on-demand" = true;
                  };
                };
              };
            in
            rec {
              "GIGA-BYTE TECHNOLOGY CO., LTD. GO27Q24G 26112F001094" = {
                mode = "2560x1440@239.901";
                position = _: {
                  props = {
                    x = 0;
                    y = 0;
                  };
                };
              }
              // vrr;
            };

          window-rules = [
            {
              geometry-corner-radius = 12;
              clip-to-geometry = true;
            }
            {
              matches = [
                { app-id = "^osu!$"; }
                { app-id = "^steam_app_[0-9]+$"; }
                { app-id = "^gamescope$"; }
              ];
              variable-refresh-rate = true;
            }
          ];

          xwayland-satellite.path = lib.getExe pkgs.xwayland-satellite;
          layer-rules = [
            {
              matches = [
                { namespace = "^wallpaper$"; }
              ];
              place-within-backdrop = true;
            }
          ];

          overview = {
            backdrop-color = "#000000";
          };

          workspaces = {
            stash = _: { };
          };

          binds = {
            "Mod+Return".spawn-sh = "wezterm";
            "Mod+Q".close-window = _: { };
            # "Mod+D".spawn-sh = "${lib.getExe self'.packages.noctalia} ipc call launcher toggle";
            "Mod+D".spawn-sh = "vicinae toggle";
            "Mod+N".spawn = [
              (lib.getExe' inputs.niri-scratchpad.packages.${pkgs.stdenv.hostPlatform.system}.default
                "niri-scratchpad"
              )
              "create"
              "1"
              "--as-float"
            ];
            "Mod+F".fullscreen-window = _: { };
            "Mod+Shift+F".toggle-window-floating = _: { };
            "Mod+Space".maximize-column = _: { };
            "Mod+Left".focus-column-left = _: { };
            "Mod+Right".focus-column-right = _: { };
            "Mod+Down".focus-window-down = _: { };
            "Mod+Up".focus-window-up = _: { };

            "Mod+Shift+Left".move-column-left = _: { };
            "Mod+Shift+Right".move-column-right = _: { };
            "Mod+Shift+Up".move-window-to-workspace-up = _: { };
            "Mod+Shift+Down".move-window-to-workspace-down = _: { };

            "Mod+Control+Down".focus-workspace-down = _: { };
            "Mod+Control+Up".focus-workspace-up = _: { };
            "Mod+WheelScrollDown".focus-column-right = _: { };
            "Mod+WheelScrollUp".focus-column-left = _: { };
            "Mod+WheelScrollLeft".focus-workspace-down = _: { };
            "Mod+Shift+WheelScrollUp".focus-workspace-up = _: { };
            "Mod+WheelScrollRight".focus-workspace-up = _: { };
            "Mod+Shift+WheelScrollDown".focus-workspace-down = _: { };
          };
        };
      };
    };
}
