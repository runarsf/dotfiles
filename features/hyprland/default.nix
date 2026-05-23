{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.hyprland = {pkgs, ...}: let
    sys = pkgs.stdenv.hostPlatform.system;
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
        package = inputs.hyprland.packages.${sys}.hyprland;
        portalPackage = inputs.hyprland.inputs.nixpkgs.legacyPackages.${sys}.xdg-desktop-portal-hyprland;
      };
    };
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      package = inputs.hyprland.inputs.nixpkgs.legacyPackages.${sys}.mesa;
      package32 = inputs.hyprland.inputs.nixpkgs.legacyPackages.${sys}.pkgsi686Linux.mesa;
    };
  };

  flake.homeModules.hyprland = {
    config,
    lib,
    pkgs,
    ...
  }: let
    sys = pkgs.stdenv.hostPlatform.system;

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

    binds = [
      "SUPER, Return, exec, ${lib.getExe pkgs.kitty}"
      "SUPER, Q, killactive"
      ''SUPER SHIFT, E, exec, loginctl terminate-user ""''
      "SUPER, E, exec, ${lib.getExe pkgs.nautilus}"
      "SUPER SHIFT, F, togglefloating"
      "SUPER ALT, F, workspaceopt, allfloat"
      "SUPER, F, fullscreen, 0"
      "SUPER, space, fullscreen, 1"
      "SUPER, A, exec, ${hypr-pin}"
      "ALT, P, exec, ${lib.getExe pkgs.grim} -g \"$(${lib.getExe pkgs.slurp})\" - | ${lib.getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${lib.getExe' pkgs.wl-clipboard "wl-copy"}"
      "ALT SHIFT, P, exec, ${lib.getExe pkgs.grim} -g \"$(${lib.getExe pkgs.slurp})\" - | ${lib.getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${lib.getExe pkgs.swappy} -f -"

      "SUPER, left, exec, ${hypr-move} focus l"
      "SUPER, right, exec, ${hypr-move} focus r"
      "SUPER, up, exec, ${hypr-move} focus u"
      "SUPER, down, exec, ${hypr-move} focus d"

      "SUPER SHIFT, TAB, centerwindow"
      "SUPER SHIFT, Return, layoutmsg, swapwithmaster"

      "SUPER, X, exec, ${lib.getExe pkgs.hyprlock}"
      "SUPER, L, exec, ${lib.getExe pkgs.hyprlock}"
      "SUPER, TAB, workspace, previous_per_monitor"

      "SUPER, C, exec, ${lib.getExe pkgs.hyprpicker} -a | tr -d '\\n' | ${lib.getExe' pkgs.wl-clipboard "wl-copy"}"
      "SUPER SHIFT, C, exec, ${hypr-gamemode} toggle"

      "SUPER, mouse_down, workspace, e+1"
      "SUPER, mouse_up, workspace, e-1"

      "SUPER, Z, exec, ${pypr} zoom ++0.5"
      "SUPER SHIFT, Z, exec, ${pypr} zoom"
      "SUPER, N, togglespecialworkspace, scratchpad"
      "SUPER SHIFT, N, exec, ${pypr} toggle_special scratchpad"
    ];

    repeatBinds = let
      wpctl = lib.getExe' pkgs.wireplumber "wpctl";
      playerctl = lib.getExe pkgs.playerctl;
      brightnessctl = lib.getExe pkgs.brightnessctl;
    in [
      "SUPER CTRL, right, resizeactive, 50 0"
      "SUPER CTRL, left, resizeactive, -50 0"
      "SUPER CTRL, up, resizeactive, 0 -50"
      "SUPER CTRL, down, resizeactive, 0 50"

      "SUPER SHIFT, right, exec, ${hypr-move} move r"
      "SUPER SHIFT, left, exec, ${hypr-move} move l"
      "SUPER SHIFT, up, exec, ${hypr-move} move u"
      "SUPER SHIFT, down, exec, ${hypr-move} move d"

      ", XF86AudioRaiseVolume, exec, ${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%+"
      ", XF86AudioLowerVolume, exec, ${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%-"
      ", XF86AudioMute, exec, ${wpctl} set-mute @DEFAULT_SINK@ toggle"
      ", XF86AudioMicMute, exec, ${wpctl} set-mute @DEFAULT_SOURCE@ toggle"
      ", XF86AudioPause, exec, ${playerctl} play-pause"
      ", XF86AudioPlay, exec, ${playerctl} play-pause"
      "SHIFT, XF86AudioMute, exec, ${playerctl} play-pause"
      ", XF86AudioNext, exec, ${playerctl} next"
      ", XF86AudioPrev, exec, ${playerctl} previous"
      ", XF86MonBrightnessUp, exec, ${brightnessctl} set 5%+"
      ", XF86MonBrightnessDown, exec, ${brightnessctl} set 5%-"
    ];

    releaseBinds = [
      "SUPER CTRL, right, exec, ${hypr-snap}"
      "SUPER CTRL, left, exec, ${hypr-snap}"
      "SUPER CTRL, up, exec, ${hypr-snap}"
      "SUPER CTRL, down, exec, ${hypr-snap}"
      "SUPER SHIFT, right, exec, ${hypr-snap}"
      "SUPER SHIFT, left, exec, ${hypr-snap}"
      "SUPER SHIFT, up, exec, ${hypr-snap}"
      "SUPER SHIFT, down, exec, ${hypr-snap}"
      "SUPER, mouse:272, exec, ${hypr-snap}"
      "SUPER, mouse:273, exec, ${hypr-snap}"
    ];

    mouseBinds = [
      "SUPER, mouse:272, movewindow"
      "SUPER SHIFT, mouse:272, movewindow"
      "SUPER, mouse:273, resizewindow"
      "SUPER SHIFT, mouse:273, resizewindow"
    ];

    windowRules = [
      "pin on, match:class (pinentry-)(.*)"
      "stay_focused on, match:class (pinentry-)(.*)"
      "pin on, match:class (gcr-prompter)"
      "stay_focused on, match:class (gcr-prompter)"
      "border_size 0, match:fullscreen 1"
      "opacity 0.8 0.8, match:class kitty"
      "opacity 0.8 0.8, match:class org.wezfurlong.wezterm"
      "no_initial_focus on, match:class ^jetbrains-(?!toolbox), match:float 1"
      "no_initial_focus on, match:class steam"
      "stay_focused on, match:title ^()$, match:class steam"
      "min_size 1 1, match:title ^()$, match:class steam"
      "workspace 4 silent, match:class steam"
      "workspace 4 silent, match:class steamwebhelper"
      "workspace 10, match:class osu!"
      "fullscreen on, match:class steam_app\\d+"
      "monitor 1, match:class steam_app_\\d+"
      "workspace 10, match:class steam_app_\\d+"
      "workspace 10, match:class gamescope"
      "workspace 2 silent, match:class (discord)"
      "workspace 2 silent, match:class (vesktop)"
      "float on, match:class (firefox)(.*), match:title (Picture-in-Picture)"
      "workspace 2, match:class (firefox)(.*), match:title (Picture-in-Picture)"
      "keep_aspect_ratio on, match:class (firefox)(.*), match:title (Picture-in-Picture)"
      "float on, match:class (firefox).*, match:title (Opening)(.*)"
      "float on, match:class (firefox).*, match:title (Save As)(.*)"
      "float on, match:class zen, match:title (Picture-in-Picture)"
      "workspace 2, match:class zen, match:title (Picture-in-Picture)"
      "dim_around on, match:class zen, match:title (Picture-in-Picture)"
      "keep_aspect_ratio on, match:class zen, match:title (Picture-in-Picture)"
      "float on, match:class zen, match:title (Opening)(.*)"
      "float on, match:class zen, match:title (Save As)(.*)"
      "pin on, match:class ssh-askpass"
      "stay_focused on, match:class ssh-askpass"
      "dim_around on, match:class ssh-askpass"
      "suppress_event maximize, match:class .*"
      "no_focus on, match:class ^$, match:title ^$, match:xwayland 1, match:float 1, match:fullscreen 0, match:pin 0"
      "float on, match:workspace name:special:scratchpad"
    ];
  in {
    imports = [inputs.hyprland.homeManagerModules.default];

    options.feature.hyprland.animations = lib.mkEnableOption "animations and dynamic cursors";

    config = {
      home.packages = with pkgs; [
        nwg-displays
        nemo
        hyprsunset
        hyprpolkitagent
        wl-clipboard
        libsForQt5.qt5.qtwayland
        pyprland
        sway-audio-idle-inhibit
        networkmanagerapplet
        grim
        slurp
        imagemagick
        swappy
        hyprpicker
        playerctl
        wireplumber
        jq
        libnotify
        brightnessctl
        kitty
      ];

      home.activation.hyprlandConfigFiles = lib.hm.dag.entryBefore ["writeBoundary"] ''
        mkdir -p "${config.home.homeDirectory}/.config/hypr"
        touch "${config.home.homeDirectory}/.config/hypr/monitors.conf" \
              "${config.home.homeDirectory}/.config/hypr/workspaces.conf"
      '';

      xdg.configFile = {
        "swaync/config.json".text = builtins.toJSON {scripts = {};};
        "hypr/pyprland.json".text = builtins.toJSON {
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
          [inputs.hyprland-plugins.packages.${sys}.borders-plus-plus]
          ++ lib.optionals config.feature.hyprland.animations [
            inputs.hypr-dynamic-cursors.packages.${sys}.hypr-dynamic-cursors
          ];
        configType = "lua";
        settings = {
          source = [
            "${config.home.homeDirectory}/.config/hypr/monitors.conf"
            "${config.home.homeDirectory}/.config/hypr/workspaces.conf"
          ];
          exec-once = [
            "uwsm finalize"
            "${lib.getExe pkgs.sway-audio-idle-inhibit}"
            "${lib.getExe pkgs.networkmanagerapplet}"
            "systemctl --user start hyprpolkitagent"
            "${hypr-gamemode}"
            "${pypr}"
          ];
          plugin.dynamic-cursors = lib.mkIf config.feature.hyprland.animations {
            enabled = true;
            mode = "tilt";
          };
          general = {
            gaps_in = 5;
            gaps_out = 20;
            border_size = 1;
            "col.active_border" = "rgba(717585FF) rgba(707480FF) 90deg";
            "col.inactive_border" = "rgba(616977FF) rgba(636973FF) 90deg";
            layout = "master";
            resize_on_border = false;
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
              tap-and-drag = true;
            };
          };
          xwayland.force_zero_scaling = true;
          master = {
            new_status = "slave";
            allow_small_split = true;
            smart_resizing = false;
          };
          dwindle = {
            pseudotile = true;
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
            animate_manual_resizes = config.feature.hyprland.animations;
            animate_mouse_windowdragging = config.feature.hyprland.animations;
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
              xray = !config.feature.hyprland.animations;
              noise = 3.0e-2;
              contrast = 1;
            };
            shadow = {
              enabled = true;
              range = 32;
              render_power = 3;
              ignore_window = true;
              scale = 1;
              color = "rgba(00000055)";
              color_inactive = "rgba(00000028)";
            };
            layerrule = [
              "blur on, match:namespace wofi"
              "blur on, match:namespace launcher"
            ];
          };
          animations = {
            enabled = "yes";
            bezier = [
              "myBezier, 0.05, 0.9, 0.1, 1.05"
              "overshot,0.05,0.9,0.1,1.1"
            ];
            animation = [
              "windows, 1, 7, overshot"
              "windowsOut, 1, 7, default, popin 80%"
              "border, 1, 10, default"
              "borderangle, 1, 8, default"
              "fade, 1, 7, default"
              "workspaces, 1, 6, default"
            ];
          };
          binds.allow_workspace_cycles = true;
          bind = binds;
          binde = repeatBinds;
          bindr = releaseBinds;
          bindm = mouseBinds;
          workspace = [
            "special:scratchpad, on-created-empty:[size 1310 836] uwsm app -- ${lib.getExe pkgs.kitty}"
          ];
          windowrule = windowRules;
        };
        extraConfig = ''
          bind=SUPER SHIFT,P,submap,passthrough
          submap=passthrough
          bind=SUPER SHIFT,P,submap,reset
          submap=reset

          ${builtins.concatStringsSep "\n" (builtins.genList (x: let
              ws = let c = (x + 1) / 10; in toString (x + 1 - (c * 10));
            in ''
              bind = SUPER, ${ws}, exec, ${hypr-workspace} ${toString (x + 1)}
              bind = SUPER SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
            '')
            10)}
        '';
      };
    };
  };
}
