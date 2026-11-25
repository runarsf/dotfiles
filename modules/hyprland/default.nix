{
  inputs,
  lib',
  ...
}:
{
  flake.nixosModules.hyprland =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
    in
    let
      hyprlandNixpkgs = inputs.hyprland.inputs.nixpkgs.legacyPackages.${system};
    in
    {
      config = lib.mkIf (config.host.desktop or true) {
        nix.settings = rec {
          substituters = [ "https://hyprland.cachix.org" ];
          trusted-substituters = substituters;
          trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
        };
        programs = {
          xwayland.enable = true;
          hyprland = {
            enable = true;
            package = inputs.hyprland.packages.${system}.hyprland;
            portalPackage = hyprlandNixpkgs.xdg-desktop-portal-hyprland;
          };
        };
        # UPDATE: keep in sync with Hyprland's nix/default.nix wrapProgram deps.
        environment.systemPackages =
          (with hyprlandNixpkgs; [
            binutils
            pciutils
            pkgconf
          ])
          ++ [
            inputs.hyprland.inputs.hyprland-guiutils.packages.${system}.hyprland-guiutils
          ];
        hardware.graphics = {
          enable = true;
          enable32Bit = true;
          package = hyprlandNixpkgs.mesa;
          package32 = hyprlandNixpkgs.pkgsi686Linux.mesa;
        };
      };
    };

  flake.homeModules.hyprland = lib'.mkModule "hyprland" {
    wants = [ "wayland" ];
    needs = [ "terminal" ];
    config =
      {
        config,
        lib,
        pkgs,
        osConfig,
        ...
      }:
      let
        inherit (lib'.hyprland) kb exec onStart lua;
        inherit (pkgs.stdenv.hostPlatform) system;
        inherit (pkgs.writers) writeNuBin;
        inherit (lib) mkDefault;

        cfg = config.modules.hyprland;

        inherit (config.lib.stylix) colors;

        hypr-snap ="${writeNuBin "hypr-snap" (builtins.readFile ./bin/hypr-snap.nu)}/bin/hypr-snap";

        hypr-gamemode = "${writeNuBin "hypr-gamemode" (builtins.readFile ./bin/hypr-gamemode.nu)}/bin/hypr-gamemode";

        hypr-pin = "${writeNuBin "hypr-pin" (builtins.readFile ./bin/hypr-pin.nu)}/bin/hypr-pin";

        hypr-workspace = "${writeNuBin "hypr-workspace" (builtins.readFile ./bin/workspace.nu)}/bin/hypr-workspace";
        hypr-fit = "${writeNuBin "hypr-fit" (builtins.readFile ./bin/fit.nu)}/bin/hypr-fit";

        pypr = lib.getExe pkgs.pyprland;

        terminal = config.modules.terminal.command;

        # Rainbow gradient, matching the niri active-window focus ring.
        rainbowColors = [
          "rgba(FF6161FF)"
          "rgba(FFD761FF)"
          "rgba(B0FF61FF)"
          "rgba(61FF88FF)"
          "rgba(61FFFFFF)"
          "rgba(6188FFFF)"
          "rgba(B061FFFF)"
          "rgba(FF61D7FF)"
          "rgba(FF6161FF)"
        ];

        borderStyles = {
          # Solid theme accent, so there is no gradient for borderangle to spin.
          minimal = {
            active.colors = [ "rgba(${colors.base0D}FF)" ];
            inactive.colors = [ "rgba(${colors.base02}FF)" ];
          };
          rainbow = {
            active = {
              colors = rainbowColors;
              angle = 45;
            };
            inactive = {
              colors = [
                "rgba(616977FF)"
                "rgba(636973FF)"
              ];
              angle = 90;
            };
          };
        };

        binds = [
          (kb "SUPER SHIFT" "P" "hl.dsp.submap(\"passthrough\")" { })
          (kb "SUPER" "Return" (exec terminal) { })
          (kb "SUPER" "Q" "hl.dsp.window.close()" { })
          (kb "SUPER SHIFT" "E" (exec "loginctl terminate-user \"\"") { })
          (kb "SUPER" "E" (exec (lib.getExe pkgs.nautilus)) { })
          (kb "SUPER SHIFT" "F" "hl.dsp.window.float({ action = \"toggle\" })" { })
          # (kb "SUPER ALT" "F" "hl.dsp.workspace.opt(\"allfloat\")" {})
          (kb "SUPER" "F" "hl.dsp.window.fullscreen({ mode = 0 })" { })
          (kb "SUPER" "A" (exec hypr-pin) { })
          (kb "ALT" "P"
            (exec ''${lib.getExe pkgs.grim} -g "$(${lib.getExe pkgs.slurp})" - | ${lib.getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${lib.getExe' pkgs.wl-clipboard "wl-copy"}'')
            { }
          )
          (kb "ALT SHIFT" "P"
            (exec ''${lib.getExe pkgs.grim} -g "$(${lib.getExe pkgs.slurp})" - | ${lib.getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${lib.getExe pkgs.swappy} -f -'')
            { }
          )

          (kb "SUPER" "left" ''function() nav.focus("left") end'' { })
          (kb "SUPER" "right" ''function() nav.focus("right") end'' { })
          (kb "SUPER" "up" ''function() nav.focus("up") end'' { })
          (kb "SUPER" "down" ''function() nav.focus("down") end'' { })

          # Layout-scoped column focus (not hl.dsp.focus): stays within the
          # workspace and doesn't cross monitors. Dispatched via exec rather than
          # directly so the bind's own dispatcher (exec_cmd) always succeeds and
          # fully consumes the scroll event, even when hl.dsp.layout has no
          # column left to move to and would otherwise let it leak through as
          # in-app scrolling.
          (kb "SUPER" "mouse_down" (exec ''hyprctl dispatch 'hl.dsp.layout("focus r")' '') { })
          (kb "SUPER" "mouse_up" (exec ''hyprctl dispatch 'hl.dsp.layout("focus l")' '') { })

          (kb "SUPER SHIFT" "TAB" "hl.dsp.window.center()" { })
          # (kb "SUPER SHIFT" "Return" "hl.dsp.layout.msg(\"swapwithmaster\")" {})

          (kb "SUPER" "X" (exec (lib.getExe pkgs.hyprlock)) { })
          (kb "SUPER" "L" (exec (lib.getExe pkgs.hyprlock)) { })
          (kb "SUPER" "TAB" "hl.dsp.focus({ workspace = \"previous_per_monitor\" })" { })

          (kb "SUPER" "C"
            (exec "${lib.getExe pkgs.hyprpicker} -a | tr -d '\\n' | ${lib.getExe' pkgs.wl-clipboard "wl-copy"}")
            { }
          )
          (kb "SUPER SHIFT" "C" (exec "${hypr-gamemode} toggle") { })

          (kb "SUPER SHIFT" "mouse_down" "hl.dsp.focus({ workspace = \"e+1\" })" { })
          (kb "SUPER SHIFT" "mouse_up" "hl.dsp.focus({ workspace = \"e-1\" })" { })

          (kb "SUPER" "Z" (exec "${pypr} zoom ++0.5") { })
          (kb "SUPER SHIFT" "Z" (exec "${pypr} zoom") { })
          (kb "SUPER" "N" "hl.dsp.workspace.toggle_special(\"scratchpad\")" { })
          (kb "SUPER SHIFT" "N" (exec "${pypr} toggle_special scratchpad") { })
        ];

        repeatBinds =
          let
            wpctl = lib.getExe' pkgs.wireplumber "wpctl";
            playerctl = lib.getExe pkgs.playerctl;
            brightnessctl = lib.getExe pkgs.brightnessctl;
          in
          [
            (kb "SUPER CTRL" "right" "hl.dsp.window.resize({ x = 50, y = 0, relative = true })" {
              repeating = true;
            })
            (kb "SUPER CTRL" "left" "hl.dsp.window.resize({ x = -50, y = 0, relative = true })" {
              repeating = true;
            })
            (kb "SUPER CTRL" "up" "hl.dsp.window.resize({ x = 0, y = -50, relative = true })" {
              repeating = true;
            })
            (kb "SUPER CTRL" "down" "hl.dsp.window.resize({ x = 0, y = 50, relative = true })" {
              repeating = true;
            })

            (kb "SUPER SHIFT" "up" ''function() nav.move("up") end'' { repeating = true; })
            (kb "SUPER SHIFT" "down" ''function() nav.move("down") end'' { repeating = true; })

            (kb "" "XF86AudioRaiseVolume" (exec "${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%+") {
              repeating = true;
            })
            (kb "" "XF86AudioLowerVolume" (exec "${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%-") {
              repeating = true;
            })
            (kb "" "XF86AudioMute" (exec "${wpctl} set-mute @DEFAULT_SINK@ toggle") { repeating = true; })
            (kb "" "XF86AudioMicMute" (exec "${wpctl} set-mute @DEFAULT_SOURCE@ toggle") { repeating = true; })
            (kb "" "XF86AudioPause" (exec "${playerctl} play-pause") { repeating = true; })
            (kb "" "XF86AudioPlay" (exec "${playerctl} play-pause") { repeating = true; })
            (kb "SHIFT" "XF86AudioMute" (exec "${playerctl} play-pause") { repeating = true; })
            (kb "" "XF86AudioNext" (exec "${playerctl} next") { repeating = true; })
            (kb "" "XF86AudioPrev" (exec "${playerctl} previous") { repeating = true; })
            (kb "" "XF86MonBrightnessUp" (exec "${brightnessctl} set 5%+") { repeating = true; })
            (kb "" "XF86MonBrightnessDown" (exec "${brightnessctl} set 5%-") { repeating = true; })
          ]
          ++ (
            lib.flatten
            <| builtins.genList (
              x:
              let
                ws =
                  let
                    c = (x + 1) / 10;
                  in
                  toString (x + 1 - (c * 10));
              in
              [
                (kb "SUPER" ws (exec "${hypr-workspace} ${toString (x + 1)}") { repeating = true; })
                (kb "SUPER SHIFT" ws "hl.dsp.window.move({ workspace = ${toString (x + 1)}, follow = false })" {
                  repeating = true;
                })
              ]
            ) 10
          );

        releaseBinds = [
          (kb "SUPER CTRL" "right" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER CTRL" "left" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER CTRL" "up" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER CTRL" "down" (exec "${hypr-snap}") { release = true; })

          (kb "SUPER SHIFT" "right" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER SHIFT" "left" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER SHIFT" "up" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER SHIFT" "down" (exec "${hypr-snap}") { release = true; })

          (kb "SUPER" "mouse:272" (exec "${hypr-snap}") { release = true; })
          (kb "SUPER" "mouse:273" (exec "${hypr-snap}") { release = true; })
        ];

        mouseBinds = [
          (kb "SUPER" "mouse:272" "hl.dsp.window.drag()" { mouse = true; })
          (kb "SUPER SHIFT" "mouse:272" "hl.dsp.window.drag()" { mouse = true; })
          (kb "SUPER" "mouse:273" "hl.dsp.window.resize()" { mouse = true; })
          (kb "SUPER SHIFT" "mouse:273" "hl.dsp.window.resize()" { mouse = true; })
        ];

        # Log rules: watch -n 0.1 "cat "/tmp/hypr/$(echo $HYPRLAND_INSTANCE_SIGNATURE)/hyprland.log" | grep -v "efresh" | grep "rule" | tail -n 40"
        windowRules = [
          {
            match = {
              class = "(pinentry-)(.*)";
            };
            pin = true;
            stay_focused = true;
          }
          {
            match = {
              class = "(gcr-prompter)";
            };
            pin = true;
            stay_focused = true;
          }

          {
            match = {
              fullscreen = true;
            };
            border_size = 0;
          }

          {
            match = {
              class = "kitty";
            };
            opacity = 0.8;
          }
          {
            match = {
              class = "org.wezfurlong.wezterm";
            };
            opacity = 0.8;
          }

          {
            match = {
              class = "^jetbrains-";
              float = true;
            };
            no_initial_focus = true;
          }
          {
            match = {
              class = "^jetbrains-toolbox$";
              float = true;
            };
            no_initial_focus = false;
          }

          # Games
          {
            match = {
              class = "steam";
            };
            no_initial_focus = true;
          }
          {
            match = {
              class = "steam";
              title = "^()$";
            };
            stay_focused = true;
            min_size = [
              1
              1
            ];
          }
          {
            match = {
              class = "steam";
            };
            workspace = "4 silent";
          }
          {
            match = {
              class = "steamwebhelper";
            };
            workspace = "4 silent";
          }
          {
            match = {
              class = "osu!";
            };
            workspace = 10;
          }
          {
            match = {
              class = "steam_app\\d+";
            };
            fullscreen = true;
          }
          {
            match = {
              class = "steam_app_\\d+";
            };
            monitor = 1;
            workspace = 10;
          }
          {
            match = {
              class = "gamescope";
            };
            workspace = 10;
          }

          # TODO A way to group these by the rule they set (all ws2 in one).
          {
            match = {
              class = "(discord)";
            };
            workspace = "2 silent";
          }
          {
            match = {
              class = "(element)";
            };
            workspace = "2 silent";
          }
          {
            match.class = "(sable-next)";
            workspace = "2 silent";
          }
          {
            match = {
              class = "(teams-for-linux)";
            };
            workspace = "2 silent";
          }
          {
            match = {
              class = "(vesktop)";
            };
            workspace = "2 silent";
          }

          {
            match = {
              class = "(firefox)(.*)";
              title = "(Picture-in-Picture)";
            };
            float = true;
            workspace = 2;
            keep_aspect_ratio = true;
          }
          {
            match = {
              class = "(firefox).*";
              title = "(Opening)(.*)";
            };
            float = true;
          }
          {
            match = {
              class = "(firefox).*";
              title = "(Save As)(.*)";
            };
            float = true;
          }

          {
            match = {
              class = "zen";
              title = "(Picture-in-Picture)";
            };
            float = true;
            workspace = 2;
            dim_around = true;
            keep_aspect_ratio = true;
          }
          {
            match = {
              class = "zen";
              title = "(Opening)(.*)";
            };
            float = true;
          }
          {
            match = {
              class = "zen";
              title = "(Save As)(.*)";
            };
            float = true;
          }

          {
            match = {
              class = "ssh-askpass";
            };
            pin = true;
            stay_focused = true;
            dim_around = true;
          }

          {
            match = {
              class = ".*";
            };
            suppress_event = "maximize";
          }
          {
            match = {
              class = "^$";
              title = "^$";
              xwayland = true;
              float = true;
              fullscreen = false;
              pin = false;
            };
            no_focus = true;
          }
          {
            match = {
              workspace = "special:scratchpad";
            };
            float = true;
          }
        ];
      in
      {
        imports = [
          inputs.hyprland.homeManagerModules.default
          ./_animations
        ];

        options.modules.hyprland = {
          extraAnimations = lib.mkEnableOption "dynamic cursors, animated manual resizes and window dragging";
          borderStyle = lib.mkOption {
            type = lib.types.enum (lib.attrNames borderStyles);
            default = "minimal";
            description = "Window border colours. `minimal` uses the stylix accent, `rainbow` the animated gradient.";
          };
          nvidia = lib.mkEnableOption "nvidia-specific environment variables" // {
            default = osConfig.modules.nvidia.enable or false;
            defaultText = lib.literalMD "whether the host enables the `nvidia` module";
          };
        };

        config = lib.mkIf (osConfig.host.desktop or true) {
          home.packages = with pkgs; [
            hyprsunset
            qt5.qtwayland
            pyprland
            inputs.nwg-displays.packages.${system}.nwg-displays
          ];

          home.activation.hyprlandConfigFiles = lib.hm.dag.entryBefore [ "writeBoundary" ] ''
            mkdir -p "${config.home.homeDirectory}/.config/hypr"
            touch "${config.home.homeDirectory}/.config/hypr/monitors.conf" \
                  "${config.home.homeDirectory}/.config/hypr/monitors.lua" \
                  "${config.home.homeDirectory}/.config/hypr/workspaces.conf" \
                  "${config.home.homeDirectory}/.config/hypr/workspaces.lua"
          '';

          xdg.configFile = {
            "swaync/config.json".text = builtins.toJSON { scripts = { }; };
            "pypr/config.toml".source = (pkgs.formats.toml { }).generate "config.toml" {
              pyprland.plugins = [
                "toggle_special"
                "magnify"
              ];
            };
          };

          wayland.windowManager.hyprland = {
            enable = true;
            systemd = {
              enable = true;
              variables = [ "--all" ];
            };
            package = null;
            portalPackage = null;
            plugins = [
              # inputs.hyprland-plugins.packages.${system}.borders-plus-plus
            ]
            ++ lib.optionals cfg.extraAnimations [
              inputs.hypr-dynamic-cursors.packages.${system}.hypr-dynamic-cursors
            ];
            configType = "lua";
            extraConfig = ''
              require("monitors")
              require("workspaces")

              ${builtins.readFile ./lua/nav.lua}

              ${builtins.readFile ./lua/focus.lua}

              -- hl.dsp.* closures are functions too, so they can't be told apart
              -- from a plain lua action; wrap them instead.
              local function dsp(dispatcher)
                return function()
                  hl.dispatch(dispatcher)
                end
              end

              local function layout_bind(bind_table)
                return function ()
                  local workspace = hl.get_active_special_workspace() or
                                    hl.get_active_workspace()

                  if not workspace then
                    return
                  end

                  local action = bind_table[workspace.tiled_layout] or bind_table.all

                  if action then
                    action()
                  end
                end
              end

              hl.bind("SUPER + space", layout_bind({
                scrolling = dsp(${exec "${hypr-fit}"}),
                all = dsp(hl.dsp.window.fullscreen({ mode = 1 })),
              }))

              hl.bind("SUPER + SHIFT + left", layout_bind({
                scrolling = dsp(hl.dsp.layout("swapcol l")),
                all = function() nav.move("left") end,
              }))

              hl.bind("SUPER + SHIFT + right", layout_bind({
                scrolling = dsp(hl.dsp.layout("swapcol r")),
                all = function() nav.move("right") end,
              }))

              hl.bind("SUPER + M", function()
                local layouts = { "scrolling", "dwindle", "master", "monocle" }
                local workspace = hl.get_active_workspace()
                if hl.get_active_special_workspace() then
                  workspace = hl.get_active_special_workspace()
                end

                local next_layout = layouts[2]

                if not workspace then
                  return
                end

                for i = 1, #layouts do
                  if layouts[i] == workspace.tiled_layout then
                    local next_layout_idx = (i % #layouts) + 1
                    next_layout = layouts[next_layout_idx]
                    break
                  end
                end

                if workspace.special then
                  hl.workspace_rule({ workspace = tostring(workspace.name), layout = next_layout })
                else
                  hl.workspace_rule({ workspace = tostring(workspace.id), layout = next_layout })
                end
              end)
            ''
            + lib.optionalString cfg.extraAnimations ''
              if hl.plugin.dynamic_cursors then
                hl.config { plugin = { dynamic_cursors = {
                  enabled = true,
                  mode = "tilt",
                  threshold = 2,
                }}}
              end
            '';
            settings = {
              env = lib.optionals cfg.nvidia [
                {
                  _args = [
                    "LIBVA_DRIVER_NAME"
                    "nvidia"
                  ];
                }
                {
                  _args = [
                    "__GLX_VENDOR_LIBRARY_NAME"
                    "nvidia"
                  ];
                }
              ];
              on = map onStart [
                "${lib.getExe pkgs.sway-audio-idle-inhibit}"
                "${lib.getExe pkgs.networkmanagerapplet}"
                "${hypr-gamemode}"
                "${pypr}"
              ]
              ++ [
                {
                  _args = [
                    "workspace.created"
                    (lua ''
                      function(ws)
                        if not ws.is_empty or ws.name ~= "special:scratchpad" then
                          return
                        end

                        hl.exec_cmd("${terminal}", {
                          float = true,
                          size = { 1200, 760 },
                          workspace = ws.name .. " silent",
                        })
                      end
                    '')
                  ];
                }
              ];
              config = {
                general = {
                  gaps_in = 5;
                  gaps_out = 20;
                  # Constant across focus states - safe, unlike a per-window
                  # override (see plugin.borders_plus_plus below for how the
                  # focused/unfocused visual distinction is actually made).
                  border_size = 2;
                  "col.active_border" = borderStyles.${cfg.borderStyle}.active;
                  "col.inactive_border" = borderStyles.${cfg.borderStyle}.inactive;
                  layout = "scrolling";
                  resize_on_border = false;
                };
                binds = {
                  allow_workspace_cycles = true;
                  # Without this, hl.dsp.focus({ direction = ... }) treats a fullscreen
                  # window as a dead end instead of cycling to the next column, which
                  # made window_direction_monitor_fallback kick in and jump monitors.
                  movefocus_cycles_fullscreen = true;
                };
                input = {
                  kb_layout = "no";
                  kb_options = "ctrl:nocaps";
                  numlock_by_default = mkDefault true;
                  accel_profile = "flat";
                  follow_mouse = 1;
                  focus_on_close = 2;
                  mouse_refocus = false;
                  touchpad = {
                    natural_scroll = true;
                    drag_lock = false;
                    tap_and_drag = true;
                  };
                };
                xwayland.force_zero_scaling = true;
                master = {
                  new_status = "slave";
                  allow_small_split = true;
                  smart_resizing = false;
                };
                dwindle = {
                  force_split = 2;
                };
                scrolling = {
                  wrap_focus = false;
                };
                # plugin.borders_plus_plus = {
                #   add_borders = 1;
                #   border_size_1 = 1;
                # };
                ecosystem.no_update_news = true;
                cursor = {
                  no_warps = true;
                };
                misc = {
                  disable_hyprland_logo = true;
                  enable_anr_dialog = false;
                  force_default_wallpaper = 0;
                  enable_swallow = true;
                  key_press_enables_dpms = true;
                  render_unfocused_fps = 30;
                  allow_session_lock_restore = 1;
                  swallow_regex = "^(Alacritty|kitty|org.wezfurlong.wezterm|com.mitchellh.ghostty)$";
                  animate_manual_resizes = cfg.extraAnimations;
                  animate_mouse_windowdragging = cfg.extraAnimations;
                };
                decoration = {
                  rounding = 9;
                  blur = {
                    enabled = true;
                    size = 16;
                    passes = 3;
                    new_optimizations = true;
                    ignore_opacity = true;
                    vibrancy = 1;
                    brightness = 1;
                    xray = !cfg.extraAnimations;
                    noise = 3.0e-2;
                    contrast = 1;
                  };
                  # shadow = {
                  #   enabled = true;
                  #   range = 32;
                  #   render_power = 3;
                  #   scale = 1;
                  #   color = "rgba(00000055)";
                  #   color_inactive = "rgba(00000028)";
                  # };
                };
              };
              layer_rule = [
                {
                  match = {
                    namespace = "wofi";
                  };
                  blur = true;
                }
                {
                  match = {
                    namespace = "launcher";
                  };
                  blur = true;
                }
              ];
              bind = binds ++ repeatBinds ++ releaseBinds ++ mouseBinds;
              window_rule = windowRules;
            };
            submaps.passthrough.settings.bind = [
              (kb "SUPER SHIFT" "P" "hl.dsp.submap(\"reset\")" { })
            ];
          };
        };
      };
  };
}
