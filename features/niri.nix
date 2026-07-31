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
      focus-workspace = pkgs.writers.writeNu "niri-focus-workspace" ''
        def main [direction: string] {
          let excluded = [${lib.concatStringsSep " " (map (name: "\"${name}\"") excludedWorkspaces)}]
          let workspaces = (niri msg -j workspaces | from json | sort-by idx)
          let windows = (niri msg -j windows | from json)
          let occupied_ids = ($windows | get workspace_id | uniq)

          let current = ($workspaces | where is_focused | first)

          let candidates = (if $direction == "down" {
            $workspaces | where idx > $current.idx
          } else {
            $workspaces | where idx < $current.idx | reverse
          } | where {|ws| ($ws.name == null) or (not ($ws.name in $excluded))})

          let named_ids = ($workspaces | where {|ws| ($ws.name != null) and (not ($ws.name in $excluded))} | get id)
          let any_named_occupied = ($named_ids | any {|id| $id in $occupied_ids})

          let filtered = if $any_named_occupied {
            $candidates | where {|ws|
              let is_empty_named = ($ws.name != null) and (not ($ws.id in $occupied_ids))
              not $is_empty_named
            }
          } else {
            $candidates
          }

          let target = if not ($filtered | is-empty) {
            $filtered | first
          } else if not ($candidates | is-empty) {
            $candidates | first
          } else {
            null
          }

          if $target != null {
            niri msg action focus-workspace $target.idx
          }
        }
      '';

      focus-or-workspace = pkgs.writers.writeNu "niri-focus-or-workspace" ''
        def main [direction: string] {
          let before = try { niri msg -j focused-window | from json | get id }
          if $before == null {
            ^${focus-workspace} $direction
            return
          }
          niri msg action $"focus-window-($direction)"
          let after = (niri msg -j focused-window | from json | get id)
          if $before == $after {
            ^${focus-workspace} $direction
          }
        }
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

      workspace-init = pkgs.writers.writeNu "niri-workspace-init" ''
        ${lib.concatStringsSep "\n" (
          lib.imap1 (
            idx: ws: "niri msg action move-workspace-to-index --reference ${ws.name} ${toString idx}"
          ) namedWorkspaces
        )}
        niri msg action focus-workspace ${toString (wsOffset + 1)}
      '';

      # TODO: Waiting on the following PR to hide the scratch WS: https://github.com/niri-wm/niri/pull/2997
      toggle-scratchpad = pkgs.writers.writeNu "niri-toggle-scratchpad" ''
        let scratchpad = (niri msg -j windows | from json | where app_id == "scratchpad" | first)

        if ($scratchpad | is-empty) {
          niri msg action spawn -- wezterm start --class scratchpad
        } else {
          let focused_workspace = (niri msg -j workspaces | from json | where is_focused | first)

          if $scratchpad.workspace_id == $focused_workspace.id {
            ^niri msg action move-window-to-workspace --window-id $scratchpad.id --focus false scratch
          } else {
            ^niri msg action move-window-to-workspace --window-id $scratchpad.id $focused_workspace.idx
            ^niri msg action focus-window --id $scratchpad.id
          }
        }
      '';
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

          # TODO: add a variable for games so we can fullscreen and move to gaming workspace without duplicating name
          window-rules = [
            {
              geometry-corner-radius = 12;
              clip-to-geometry = true;
            }
            {
              matches = [
                { app-id = "^osu!$"; }
                { app-id = "^steam_app_[0-9]+$"; }
                { app-id = "^valheim\\.x86_64$"; }
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
                { app-id = "^valheim\\.x86_64$"; }
                { app-id = "^osu!$"; }
              ];
              open-on-workspace = "gaming";
            }
            {
              matches = [
                { app-id = "^steam_app_[0-9]+$"; }
                { app-id = "^valheim\\.x86_64$"; }
                { app-id = "^osu!$"; }
              ];
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

          "spawn-at-startup" = [ [ "${workspace-init}" ] ];

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

          workspaces = builtins.listToAttrs namedWorkspaces;

          # Run `niri msg action` to see a list of commands
          binds =
            let
              wpctl = lib.getExe' pkgs.wireplumber "wpctl";
              playerctl = lib.getExe pkgs.playerctl;
              brightnessctl = lib.getExe pkgs.brightnessctl;
            in
            {
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
