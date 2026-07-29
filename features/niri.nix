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
  flake.nixosModules.niri = { pkgs, ... }: {
    programs.niri = {
      enable = true;
      package = self.packages.${pkgs.stdenv.hostPlatform.system}.niri;
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
    let
      focus-or-workspace = pkgs.writeTextFile {
        name = "niri-focus-or-workspace";
        executable = true;
        text = ''
          #!${pkgs.nushell}/bin/nu

          def main [direction: string] {
            let before = try { niri msg -j focused-window | from json | get id }
            if $before == null {
              niri msg action $"focus-workspace-($direction)"
              return
            }
            niri msg action $"focus-window-($direction)"
            let after = (niri msg -j focused-window | from json | get id)
            if $before == $after {
              niri msg action $"focus-workspace-($direction)"
            }
          }
        '';
      };

      toggle-scratchpad = pkgs.writeTextFile {
        name = "niri-toggle-scratchpad";
        executable = true;
        text = ''
          #!${pkgs.nushell}/bin/nu

          let windows = (niri msg -j windows | from json)
          let matching = ($windows | where app_id == "scratchpad")

          if ($matching | is-empty) {
            niri msg action spawn -- wezterm start --class scratchpad
          } else {
            let window = ($matching | first)
            let workspaces = (niri msg -j workspaces | from json)
            let focused = ($workspaces | where is_focused | first)

            if $window.workspace_id == $focused.id {
              let occupied_ids = ($windows | get workspace_id | uniq)
              let hide_ws = ($workspaces | where {|ws| $ws.id not-in $occupied_ids} | last)
              ^niri msg action move-window-to-workspace --window-id $window.id --focus false $hide_ws.idx
            } else {
              ^niri msg action move-window-to-workspace --window-id $window.id $focused.idx
              ^niri msg action focus-window --id $window.id
            }
          }
        '';
      };
    in
    {
      # TODO: polkit and portal https://github.com/niri-wm/niri/wiki/Important-Software
      packages.niri = inputs.wrapper-modules.wrappers.niri.wrap {
        inherit pkgs;

        # extraSettings = [
        #   {
        #     include = [
        #       { optional = true; }
        #       "~/.config/niri/monitor.kdl"
        #     ];
        #   }
        # ];
        settings = {
          prefer-no-csd = _: { };

          input = {
            keyboard = {
              xkb.layout = "no";
              repeat-delay = 200;
              repeat-rate = 35;
            };
            mouse = {
              accel-profile = "flat";
            };
            focus-follows-mouse = _: { props = { max-scroll-amount = "0%"; }; };
            touchpad = {
              natural-scroll = _: { };
              tap = _: { };
            };
          };

          layout = {
            gaps = 10;

            background-color = "transparent";
            center-focused-column = "on-overflow";
            always-center-single-column = _: { };

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
              urgent-gradient = _: {
                props = {
                  from = "#ff2040";
                  to = "#ff5530";
                  angle = 45;
                  "in" = "oklch";
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
            {
              matches = [ { app-id = "^scratchpad$"; } ];
              open-floating = true;
            }
            {
              matches = [
                { app-id = "^steam$"; }
                { app-id = "^steam_app_[0-9]+$"; }
              ];
              open-on-workspace = "gaming";
            }
            {
              matches = [ { app-id = "^steam_app_[0-9]+$"; } ];
              open-fullscreen = true;
            }
            {
              matches = [
                { app-id = "^discord$"; }
                { app-id = "^[Ee]lement"; }
              ];
              open-on-workspace = "chat";
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
            gaming = _: { };
            chat = _: { };
            scratch = _: { };
          };

          # Run `niri msg action` to see a list of commands
          binds = let
            wpctl = lib.getExe' pkgs.wireplumber "wpctl";
            playerctl = lib.getExe pkgs.playerctl;
            brightnessctl = lib.getExe pkgs.brightnessctl;
          in {
            "Mod+Return".spawn-sh = "wezterm";
            "Mod+Q".close-window = _: { };
            # "Mod+D".spawn-sh = "${lib.getExe self'.packages.noctalia} ipc call launcher toggle";
            "Mod+D".spawn-sh = "vicinae toggle";
            "Mod+N".spawn-sh = "${toggle-scratchpad}";
            "Mod+F".fullscreen-window = _: { };
            "Mod+Shift+F".toggle-window-floating = _: { };
            "Mod+Space".maximize-column = _: { };
            "Mod+Left".focus-column-left = _: { };
            "Mod+Right".focus-column-right = _: { };
            "Mod+Down".spawn = [ "${focus-or-workspace}" "down" ];
            "Mod+Up".spawn = [ "${focus-or-workspace}" "up" ];
            "Mod+Shift+E".quit = _: { };

            "Mod+Shift+Left".move-column-left = _: { };
            "Mod+Shift+Right".move-column-right = _: { };
            "Mod+Shift+Up".move-column-to-workspace-up = _: { };
            "Mod+Shift+Down".move-column-to-workspace-down = _: { };
            "Mod+Shift+Control+Up".move-window-to-workspace-up = _: { };
            "Mod+Shift+Control+Down".move-window-to-workspace-down = _: { };

            "Alt+P".screenshot = _: { };

            "Mod+1".focus-workspace = 1;
            "Mod+2".focus-workspace = 2;
            "Mod+3".focus-workspace = 3;
            "Mod+4".focus-workspace = 4;
            "Mod+5".focus-workspace = 5;
            "Mod+6".focus-workspace = 6;
            "Mod+7".focus-workspace = 7;
            "Mod+8".focus-workspace = 8;
            "Mod+9".focus-workspace = 9;
            "Mod+0".focus-workspace = 10;

            "Mod+Shift+1".move-column-to-workspace = 1;
            "Mod+Shift+2".move-column-to-workspace = 2;
            "Mod+Shift+3".move-column-to-workspace = 3;
            "Mod+Shift+4".move-column-to-workspace = 4;
            "Mod+Shift+5".move-column-to-workspace = 5;
            "Mod+Shift+6".move-column-to-workspace = 6;
            "Mod+Shift+7".move-column-to-workspace = 7;
            "Mod+Shift+8".move-column-to-workspace = 8;
            "Mod+Shift+9".move-column-to-workspace = 9;
            "Mod+Shift+0".move-column-to-workspace = 10;

            "Mod+Shift+Control+1".move-window-to-workspace = 1;
            "Mod+Shift+Control+2".move-window-to-workspace = 2;
            "Mod+Shift+Control+3".move-window-to-workspace = 3;
            "Mod+Shift+Control+4".move-window-to-workspace = 4;
            "Mod+Shift+Control+5".move-window-to-workspace = 5;
            "Mod+Shift+Control+6".move-window-to-workspace = 6;
            "Mod+Shift+Control+7".move-window-to-workspace = 7;
            "Mod+Shift+Control+8".move-window-to-workspace = 8;
            "Mod+Shift+Control+9".move-window-to-workspace = 9;
            "Mod+Shift+Control+0".move-window-to-workspace = 10;

            "Mod+Control+Right".set-column-width = "+3%";
            "Mod+Control+Left".set-column-width = "-3%";
            "Mod+Control+Down".set-window-height = "+3%";
            "Mod+Control+Up".set-window-height = "-3%";

            "Mod+WheelScrollDown".focus-column-right = _: { };
            "Mod+WheelScrollUp".focus-column-left = _: { };
            "Mod+WheelScrollLeft".focus-workspace-down = _: { };
            "Mod+Shift+WheelScrollUp".focus-workspace-up = _: { };
            "Mod+WheelScrollRight".focus-workspace-up = _: { };
            "Mod+Shift+WheelScrollDown".focus-workspace-down = _: { };

            "XF86AudioRaiseVolume".spawn-sh = "${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%+";
            "XF86AudioLowerVolume".spawn-sh = "${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%-";
            "XF86AudioMute".spawn-sh = "${wpctl} set-mute @DEFAULT_SINK@ toggle";
            "XF86AudioMicMute".spawn-sh = "${wpctl} set-mute @DEFAULT_SOURCE@ toggle";
            "XF86AudioPause".spawn-sh = "${playerctl} play-pause";
            "XF86AudioPlay".spawn-sh = "${playerctl} play-pause";
            "Shift+XF86AudioMute".spawn-sh = "${playerctl} play-pause";
            "XF86AudioNext".spawn-sh = "${playerctl} next";
            "XF86AudioPrev".spawn-sh = "${playerctl} previous";
            "XF86MonBrightnessUp".spawn-sh = "${brightnessctl} set 5%+";
            "XF86MonBrightnessDown".spawn-sh = "${brightnessctl} set 5%-";
          };
        };
      };
    };
}
