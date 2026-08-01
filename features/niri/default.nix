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
      focus-workspace = pkgs.writeShellScript "niri-focus-workspace" ''
        export NIRI_EXCLUDED_WORKSPACES="${lib.concatStringsSep " " excludedWorkspaces}"
        exec ${pkgs.nushell}/bin/nu ${./bin/focus-workspace.nu} "$@"
      '';

      focus-or-workspace = pkgs.writeShellScript "niri-focus-or-workspace" ''
        export NIRI_FOCUS_WORKSPACE="${focus-workspace}"
        exec ${pkgs.nushell}/bin/nu ${./bin/focus-or-workspace.nu} "$@"
      '';

      namedWorkspaces = [
        {
          name = "scratch";
          value = _: { layout.background-color = "#242424"; };
        }
        {
          name = "gaming";
          value = _: { layout.background-color = "#4a1e1e"; };
        }
        {
          name = "chat";
          value = _: { layout.background-color = "#1e3a4a"; };
        }
      ];
      wsOffset = builtins.length namedWorkspaces;
      excludedWorkspaces = [ "scratch" ];

      workspace-init = pkgs.writeShellScript "niri-workspace-init" ''
        export NIRI_NAMED_WORKSPACES="${lib.concatStringsSep " " (map (ws: ws.name) namedWorkspaces)}"
        export NIRI_WS_OFFSET="${toString wsOffset}"
        exec ${pkgs.nushell}/bin/nu ${./bin/workspace-init.nu}
      '';

      # TODO: Waiting on the following PR to hide the scratch WS: https://github.com/niri-wm/niri/pull/2997
      toggle-scratchpad = pkgs.writeShellScript "niri-toggle-scratchpad" ''
        exec ${pkgs.nushell}/bin/nu ${./bin/toggle-scratchpad.nu}
      '';

      steam-game-handler = pkgs.writeShellScript "niri-steam-game-handler" ''
        export PATH="${pkgs.xdotool}/bin:$PATH"
        exec ${pkgs.nushell}/bin/nu ${./bin/steam-game-handler.nu}
      '';

      floating-sidebar = pkgs.writeShellScript "niri-floating-sidebar" ''
        exec ${pkgs.bash}/bin/bash ${./bin/floating-sidebar.sh} "$@"
      '';

      master-stack = pkgs.writeShellScript "niri-master-stack" ''
        exec ${pkgs.bash}/bin/bash ${./bin/master-stack.sh} "$@"
      '';

      gameMatches = [
        { app-id = "^osu!$"; }
        { app-id = "^r2modman$"; }
        { app-id = "^steam_app_[0-9]+$"; }
      ];
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
            focus-follows-mouse = _: {
              props = {
                max-scroll-amount = "50%";
              };
            };
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
            # empty-workspace-above-first = _: { };

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
              matches = gameMatches ++ [ { app-id = "^gamescope$"; } ];
              variable-refresh-rate = true;
            }
            {
              matches = [ { app-id = "^scratchpad$"; } ];
              open-floating = true;
            }
            {
              matches = [ { app-id = "^steam$"; } ] ++ gameMatches;
              open-on-workspace = "gaming";
            }
            {
              matches = gameMatches;
              open-fullscreen = true;
            }
            {
              matches = [
                { app-id = "^discord$"; }
                { app-id = "^element$"; }
              ];
              open-on-workspace = "chat";
            }
            {
              matches = [
                { app-id = "^steam$"; }
                { app-id = "^zen$"; }
              ];
              open-maximized = true;
            }
          ];

          "spawn-at-startup" = [
            [ "${workspace-init}" ]
            [ "${steam-game-handler}" ]
            [ "${floating-sidebar}" "listen" ]
          ];

          xwayland-satellite.path = lib.getExe pkgs.xwayland-satellite;
          layer-rules = [
            {
              matches = [
                { namespace = "^wallpaper$"; }
                { namespace = "^hyprpaper$"; }
              ];
              place-within-backdrop = true;
            }
          ];

          overview = {
            backdrop-color = "#000000";
          };

          workspaces = builtins.listToAttrs namedWorkspaces;

          # Run `niri msg action` to see a list of commands
          binds =
            let
              wpctl = lib.getExe' pkgs.wireplumber "wpctl";
              playerctl = lib.getExe pkgs.playerctl;
              brightnessctl = lib.getExe pkgs.brightnessctl;
            in
            {
              "Mod+A".spawn = [ "${floating-sidebar}" "toggle" ];
              "Mod+S".spawn = [ "${floating-sidebar}" "hide" ];
              "Mod+I".spawn = [ "${floating-sidebar}" "flip" ];
              "Mod+M".spawn = [ "${master-stack}" ];

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
              "Mod+Down".spawn = [
                "${focus-or-workspace}"
                "down"
              ];
              "Mod+Up".spawn = [
                "${focus-or-workspace}"
                "up"
              ];
              "Mod+Shift+E".quit = _: { };

              "Mod+Shift+Left".move-column-left = _: { };
              "Mod+Shift+Right".move-column-right = _: { };
              "Mod+Shift+Up".move-column-to-workspace-up = _: { };
              "Mod+Shift+Down".move-column-to-workspace-down = _: { };
              "Mod+Shift+Control+Up".move-window-to-workspace-up = _: { };
              "Mod+Shift+Control+Down".move-window-to-workspace-down = _: { };

              "Alt+P".screenshot = _: { };

              "Mod+Control+Right".set-column-width = "+3%";
              "Mod+Control+Left".set-column-width = "-3%";
              "Mod+Control+Down".set-window-height = "+3%";
              "Mod+Control+Up".set-window-height = "-3%";

              "Mod+WheelScrollDown".focus-column-right = _: { };
              "Mod+WheelScrollUp".focus-column-left = _: { };
              "Mod+WheelScrollLeft".spawn = [ "${focus-workspace}" "down" ];
              "Mod+Shift+WheelScrollUp".spawn = [ "${focus-workspace}" "up" ];
              "Mod+WheelScrollRight".spawn = [ "${focus-workspace}" "up" ];
              "Mod+Shift+WheelScrollDown".spawn = [ "${focus-workspace}" "down" ];

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
            }
            // builtins.foldl' (
              acc: n:
              let
                key = if n == 10 then "0" else toString n;
              in
              acc
              // {
                "Mod+${key}"."focus-workspace" = wsOffset + n;
                "Mod+Shift+${key}"."move-column-to-workspace" = wsOffset + n;
                "Mod+Shift+Control+${key}"."move-window-to-workspace" = wsOffset + n;
              }
            ) { } (lib.range 1 10);
        };
      };
    };
}
