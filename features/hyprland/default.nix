{
  inputs,
  lib',
  ...
}: {
  flake.nixosModules.hyprland = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    nix.settings = rec {
      substituters = ["https://hyprland.cachix.org"];
      trusted-substituters = substituters;
      trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
    };
    programs = {
      xwayland.enable = true;
      uwsm.enable = true;
      hyprland = {
        enable = true;
        withUWSM = true;
        package = inputs.hyprland.packages.${system}.hyprland;
        portalPackage = inputs.hyprland.inputs.nixpkgs.legacyPackages.${system}.xdg-desktop-portal-hyprland;
      };
    };
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      package = inputs.hyprland.inputs.nixpkgs.legacyPackages.${system}.mesa;
      package32 = inputs.hyprland.inputs.nixpkgs.legacyPackages.${system}.pkgsi686Linux.mesa;
    };
  };

  flake.homeModules.hyprland = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib'.uwsm) run runOnce;
    inherit (lib'.hyprland) kb exec onStart;
    inherit (pkgs.stdenv.hostPlatform) system;

    cfg = config.features.hyprland;

    hypr-snap = pkgs.writers.writePython3 "hypr-snap" {
      flakeIgnore = ["E305" "E501" "E227" "E302" "E225"];
    } (builtins.readFile ./bin/hypr-snap.py);

    hypr-gamemode = pkgs.writers.writePython3 "hypr-gamemode" {
      flakeIgnore = ["E305" "E501" "E227" "E302" "E225" "E731"];
    } (builtins.readFile ./bin/hypr-gamemode.py);

    hypr-pin = "${pkgs.writeShellApplication {
      name = "hypr-pin";
      runtimeInputs = with pkgs; [jq libnotify];
      text = builtins.readFile ./bin/hypr-pin.sh;
    }}/bin/hypr-pin";

    hypr-move = "${pkgs.writers.writeNuBin "hypr-move" (builtins.readFile ./bin/move.nu)}/bin/hypr-move";
    hypr-workspace = "${pkgs.writers.writeNuBin "hypr-workspace" (builtins.readFile ./bin/workspace.nu)}/bin/hypr-workspace";

    pypr = lib.getExe pkgs.pyprland;
    terminal = config.programs.wezterm.package;

    binds = [
      (kb "SUPER SHIFT" "P" "hl.dsp.submap(\"passthrough\")" {})
      (kb "SUPER" "Return" (exec (run (lib.getExe terminal))) {})
      (kb "SUPER" "Q" "hl.dsp.window.close()" {})
      (kb "SUPER SHIFT" "E" (exec "loginctl terminate-user \"\"") {})
      (kb "SUPER" "E" (exec (run (lib.getExe pkgs.nautilus))) {})
      (kb "SUPER SHIFT" "F" "hl.dsp.window.float({ action = \"toggle\" })" {})
      # (kb "SUPER ALT" "F" "hl.dsp.workspace.opt(\"allfloat\")" {})
      (kb "SUPER" "F" "hl.dsp.window.fullscreen({ mode = 0 })" {})
      (kb "SUPER" "space" "hl.dsp.window.fullscreen({ mode = 1 })" {})
      (kb "SUPER" "A" (exec (run hypr-pin)) {})
      (kb "ALT" "P" (exec (run ''${lib.getExe pkgs.grim} -g "$(${lib.getExe pkgs.slurp})" - | ${lib.getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${lib.getExe' pkgs.wl-clipboard "wl-copy"}'')) {})
      (kb "ALT SHIFT" "P" (exec (run ''${lib.getExe pkgs.grim} -g "$(${lib.getExe pkgs.slurp})" - | ${lib.getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${lib.getExe pkgs.swappy} -f -'')) {})

      (kb "SUPER" "left" (exec "${hypr-move} focus l") {})
      (kb "SUPER" "right" (exec "${hypr-move} focus r") {})
      (kb "SUPER" "up" (exec "${hypr-move} focus u") {})
      (kb "SUPER" "down" (exec "${hypr-move} focus d") {})

      (kb "SUPER SHIFT" "TAB" "hl.dsp.window.center()" {})
      # (kb "SUPER SHIFT" "Return" "hl.dsp.layout.msg(\"swapwithmaster\")" {})

      (kb "SUPER" "X" (exec (runOnce (lib.getExe pkgs.hyprlock))) {})
      (kb "SUPER" "L" (exec (runOnce (lib.getExe pkgs.hyprlock))) {})
      (kb "SUPER" "TAB" "hl.dsp.focus({ workspace = \"previous_per_monitor\" })" {})

      (kb "SUPER" "C" (exec "${lib.getExe pkgs.hyprpicker} -a | tr -d '\\n' | ${lib.getExe' pkgs.wl-clipboard "wl-copy"}") {})
      (kb "SUPER SHIFT" "C" (exec "${hypr-gamemode} toggle") {})

      (kb "SUPER" "mouse_down" "hl.dsp.focus({ workspace = \"e+1\" })" {})
      (kb "SUPER" "mouse_up" "hl.dsp.focus({ workspace = \"e-1\" })" {})

      (kb "SUPER" "Z" (exec "${pypr} zoom ++0.5") {})
      (kb "SUPER SHIFT" "Z" (exec "${pypr} zoom") {})
      (kb "SUPER" "N" "hl.dsp.workspace.toggle_special(\"scratchpad\")" {})
      (kb "SUPER SHIFT" "N" (exec "${pypr} toggle_special scratchpad") {})
    ];

    repeatBinds = let
      wpctl = lib.getExe' pkgs.wireplumber "wpctl";
      playerctl = lib.getExe pkgs.playerctl;
      brightnessctl = lib.getExe pkgs.brightnessctl;
    in
      [
        (kb "SUPER CTRL" "right" "hl.dsp.window.resize({ x = 50, y = 0, relative = true })" {repeating = true;})
        (kb "SUPER CTRL" "left" "hl.dsp.window.resize({ x = -50, y = 0, relative = true })" {repeating = true;})
        (kb "SUPER CTRL" "up" "hl.dsp.window.resize({ x = 0, y = -50, relative = true })" {repeating = true;})
        (kb "SUPER CTRL" "down" "hl.dsp.window.resize({ x = 0, y = 50, relative = true })" {repeating = true;})

        (kb "SUPER SHIFT" "right" (exec "${hypr-move} move r") {repeating = true;})
        (kb "SUPER SHIFT" "left" (exec "${hypr-move} move l") {repeating = true;})
        (kb "SUPER SHIFT" "up" (exec "${hypr-move} move u") {repeating = true;})
        (kb "SUPER SHIFT" "down" (exec "${hypr-move} move d") {repeating = true;})

        (kb "" "XF86AudioRaiseVolume" (exec "${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%+") {repeating = true;})
        (kb "" "XF86AudioLowerVolume" (exec "${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%-") {repeating = true;})
        (kb "" "XF86AudioMute" (exec "${wpctl} set-mute @DEFAULT_SINK@ toggle") {repeating = true;})
        (kb "" "XF86AudioMicMute" (exec "${wpctl} set-mute @DEFAULT_SOURCE@ toggle") {repeating = true;})
        (kb "" "XF86AudioPause" (exec "${playerctl} play-pause") {repeating = true;})
        (kb "" "XF86AudioPlay" (exec "${playerctl} play-pause") {repeating = true;})
        (kb "SHIFT" "XF86AudioMute" (exec "${playerctl} play-pause") {repeating = true;})
        (kb "" "XF86AudioNext" (exec "${playerctl} next") {repeating = true;})
        (kb "" "XF86AudioPrev" (exec "${playerctl} previous") {repeating = true;})
        (kb "" "XF86MonBrightnessUp" (exec "${brightnessctl} set 5%+") {repeating = true;})
        (kb "" "XF86MonBrightnessDown" (exec "${brightnessctl} set 5%-") {repeating = true;})
      ]
      ++ (lib.flatten
        <| builtins.genList (x: let
          ws = let c = (x + 1) / 10; in toString (x + 1 - (c * 10));
        in [
          (kb "SUPER" ws (exec "${hypr-workspace} ${toString (x + 1)}") {repeating = true;})
          (kb "SUPER SHIFT" ws "hl.dsp.window.move({ workspace = ${toString (x + 1)} })" {repeating = true;})
        ])
        10);

    releaseBinds = [
      (kb "SUPER CTRL" "right" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER CTRL" "left" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER CTRL" "up" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER CTRL" "down" (exec "${hypr-snap}") {release = true;})

      (kb "SUPER SHIFT" "right" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER SHIFT" "left" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER SHIFT" "up" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER SHIFT" "down" (exec "${hypr-snap}") {release = true;})

      (kb "SUPER" "mouse:272" (exec "${hypr-snap}") {release = true;})
      (kb "SUPER" "mouse:273" (exec "${hypr-snap}") {release = true;})
    ];

    mouseBinds = [
      (kb "SUPER" "mouse:272" "hl.dsp.window.drag()" {mouse = true;})
      (kb "SUPER SHIFT" "mouse:272" "hl.dsp.window.drag()" {mouse = true;})
      (kb "SUPER" "mouse:273" "hl.dsp.window.resize()" {mouse = true;})
      (kb "SUPER SHIFT" "mouse:273" "hl.dsp.window.resize()" {mouse = true;})
    ];

    # Log rules: watch -n 0.1 "cat "/tmp/hypr/$(echo $HYPRLAND_INSTANCE_SIGNATURE)/hyprland.log" | grep -v "efresh" | grep "rule" | tail -n 40"
    windowRules = [
      {
        match = {class = "(pinentry-)(.*)";};
        pin = true;
        stay_focused = true;
      }
      {
        match = {class = "(gcr-prompter)";};
        pin = true;
        stay_focused = true;
      }

      {
        match = {fullscreen = true;};
        border_size = 0;
      }

      {
        match = {class = "kitty";};
        opacity = 0.8;
      }
      {
        match = {class = "org.wezfurlong.wezterm";};
        opacity = 0.8;
      }

      {
        match = {
          class = "^jetbrains-(?!toolbox)";
          float = true;
        };
        no_initial_focus = true;
      }

      # Games
      {
        match = {class = "steam";};
        no_initial_focus = true;
      }
      {
        match = {
          class = "steam";
          title = "^()$";
        };
        stay_focused = true;
        min_size = [1 1];
      }
      {
        match = {class = "steam";};
        workspace = "4 silent";
      }
      {
        match = {class = "steamwebhelper";};
        workspace = "4 silent";
      }
      {
        match = {class = "osu!";};
        workspace = 10;
      }
      {
        match = {class = "steam_app\\d+";};
        fullscreen = true;
      }
      {
        match = {class = "steam_app_\\d+";};
        monitor = 1;
        workspace = 10;
      }
      {
        match = {class = "gamescope";};
        workspace = 10;
      }

      {
        match = {class = "(discord)";};
        workspace = "2 silent";
      }
      {
        match = {class = "(vesktop)";};
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
        match = {class = "ssh-askpass";};
        pin = true;
        stay_focused = true;
        dim_around = true;
      }

      {
        match = {class = ".*";};
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
        match = {workspace = "name:special:scratchpad";};
        float = true;
      }
    ];
  in {
    imports = [inputs.hyprland.homeManagerModules.default];

    options.features.hyprland.animations = lib.mkEnableOption "animations and dynamic cursors";

    config = {
      home.packages = with pkgs; [
        hyprsunset
        libsForQt5.qt5.qtwayland
        pyprland
        inputs.nwg-displays.packages.${system}.nwg-displays
      ];

      home.activation.hyprlandConfigFiles = lib.hm.dag.entryBefore ["writeBoundary"] ''
        mkdir -p "${config.home.homeDirectory}/.config/hypr"
        touch "${config.home.homeDirectory}/.config/hypr/monitors.conf" \
              "${config.home.homeDirectory}/.config/hypr/monitors.lua" \
              "${config.home.homeDirectory}/.config/hypr/workspaces.conf" \
              "${config.home.homeDirectory}/.config/hypr/workspaces.lua"
      '';

      xdg.configFile = {
        # UWSM reads this before launching the session; it must also re-export
        # hm-session-vars so home-manager variables survive the UWSM handoff.
        "uwsm/env".text =
          ''
            export NIXOS_OZONE_WL=1
            export MOZ_ENABLE_WAYLAND=1
          ''
          + "\n"
          + builtins.readFile "${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh";

        "swaync/config.json".text = builtins.toJSON {scripts = {};};
        "pypr/config.toml".source = (pkgs.formats.toml {}).generate "config.toml" {
          pyprland.plugins = ["toggle_special" "magnify"];
        };
      };

      wayland.windowManager.hyprland = {
        enable = true;
        systemd = {
          enable = false;
          variables = ["--all"];
        };
        package = null;
        portalPackage = null;
        plugins =
          [inputs.hyprland-plugins.packages.${system}.borders-plus-plus]
          ++ lib.optionals cfg.animations [
            inputs.hypr-dynamic-cursors.packages.${system}.hypr-dynamic-cursors
          ];
        configType = "lua";
        extraConfig = ''
          require("monitors")
          require("workspaces")
        '';
        settings = {
          on = map onStart [
            "uwsm finalize"
            "${lib.getExe pkgs.sway-audio-idle-inhibit}"
            "${lib.getExe pkgs.networkmanagerapplet}"
            "systemctl --user start hyprpolkitagent"
            "${hypr-gamemode}"
            "${pypr}"
          ];
          config = {
            general = {
              gaps_in = 5;
              gaps_out = 20;
              border_size = 1;
              "col.active_border" = {
                colors = ["rgba(717585FF)" "rgba(707480FF)"];
                angle = 90;
              };
              "col.inactive_border" = {
                colors = ["rgba(616977FF)" "rgba(636973FF)"];
                angle = 90;
              };
              layout = "master";
              resize_on_border = false;
            };
            binds.allow_workspace_cycles = true;
            plugin.dynamic-cursors = lib.mkIf cfg.animations {
              enabled = true;
              mode = "tilt";
            };
            input = {
              kb_layout = "no";
              kb_options = "ctrl:nocaps";
              numlock_by_default = true;
              accel_profile = "flat";
              follow_mouse = 1;
              mouse_refocus = false;
              sensitivity = 0.5;
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
            ecosystem.no_update_news = true;
            misc = {
              disable_hyprland_logo = true;
              enable_anr_dialog = false;
              force_default_wallpaper = 0;
              enable_swallow = true;
              key_press_enables_dpms = true;
              render_unfocused_fps = 30;
              allow_session_lock_restore = 1;
              swallow_regex = "^(Alacritty|kitty|org.wezfurlong.wezterm|com.mitchellh.ghostty)$";
              animate_manual_resizes = cfg.animations;
              animate_mouse_windowdragging = cfg.animations;
            };
            decoration = {
              rounding = 7;
              blur = {
                enabled = true;
                size = 16;
                passes = 3;
                new_optimizations = true;
                ignore_opacity = true;
                vibrancy = 1;
                brightness = 1;
                xray = !cfg.animations;
                noise = 3.0e-2;
                contrast = 1;
              };
              shadow = {
                enabled = true;
                range = 32;
                render_power = 3;
                scale = 1;
                color = "rgba(00000055)";
                color_inactive = "rgba(00000028)";
              };
            };
          };
          layer_rule = [
            {
              match = {namespace = "wofi";};
              blur = true;
            }
            {
              match = {namespace = "launcher";};
              blur = true;
            }
          ];
          curve = [
            {
              _args = [
                "myBezier"
                {
                  type = "bezier";
                  points = [[0.05 0.9] [0.1 1.05]];
                }
              ];
            }
            {
              _args = [
                "overshot"
                {
                  type = "bezier";
                  points = [[0.05 0.9] [0.1 1.1]];
                }
              ];
            }
          ];
          animation = [
            {
              leaf = "windows";
              enabled = true;
              speed = 7;
              bezier = "overshot";
            }
            {
              leaf = "windowsOut";
              enabled = true;
              speed = 7;
              bezier = "default";
              style = "popin 80%";
            }
            {
              leaf = "border";
              enabled = true;
              speed = 10;
              bezier = "default";
            }
            {
              leaf = "borderangle";
              enabled = true;
              speed = 8;
              bezier = "default";
            }
            {
              leaf = "fade";
              enabled = true;
              speed = 7;
              bezier = "default";
            }
            {
              leaf = "workspaces";
              enabled = true;
              speed = 6;
              bezier = "default";
            }
          ];
          bind = binds ++ repeatBinds ++ releaseBinds ++ mouseBinds;
          workspace_rule = [
            {
              workspace = "special:scratchpad";
              on_created_empty = "[size 1310 836] ${run (lib.getExe terminal)}";
            }
          ];
          window_rule = windowRules;
        };
        submaps.passthrough.settings.bind = [
          (kb "SUPER SHIFT" "P" "hl.dsp.submap(\"reset\")" {})
        ];
      };
    };
  };
}
