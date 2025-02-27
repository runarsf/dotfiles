{
  config,
  inputs,
  outputs,
  pkgs,
  system,
  ...
}:
# TODO https://wiki.hyprland.org/Useful-Utilities/Systemd-start/
# TODO temporary command (nix-shell) with fuzzel
# TODO Better switching of layouts (master, centered master, dwindle, pseudo) https://wiki.hyprland.org/Configuring/Master-Layout/#workspace-rules
# TODO See if smart_resizing negates the need for a resizing script
# TODO refactor master layout config
# TODO Scratchpad with tags https://wiki.hyprland.org/Configuring/Window-Rules/#tags
let
  lock = "${pkgs.hyprlock}/bin/hyprlock";
  hypr-snap = pkgs.writers.writePython3 "hypr-snap" {
    flakeIgnore = [
      "E305"
      "E501"
      "E227"
      "E302"
      "E225"
    ];
  } (builtins.readFile ./bin/hypr-snap.py);
  hypr-gamemode = pkgs.writers.writePython3 "hypr-gamemode" {
    flakeIgnore = [
      "E305"
      "E501"
      "E227"
      "E302"
      "E225"
      "E731"
    ];
  } (builtins.readFile ./bin/hypr-gamemode.py);
in
  {
    imports = [
      # inputs.hyprland.nixosModules.default
      inputs.hyprland.homeManagerModules.default
      ./hypridle.nix
      ./hyprlock.nix
      ./hyprpaper.nix
      ./hyprpanel.nix
      ./pyprland.nix
    ];
  }
  // outputs.lib.mkDesktopModule' config "hyprland"
  {
    animations = outputs.lib.mkEnableOption "Enable funky animations";
  }
  {
    modules = outputs.lib.enable [
      "wayland"
      "fuzzel"
      "hyprpanel"
      "hypridle"
      "hyprlock"
      "hyprpaper"
      "pyprland"
      # "ulauncher"
      # "waybar"
    ];
    services = outputs.lib.enable ["kanshi"]; # swaync
    # programs.jq.enable = true;

    home.packages = with pkgs; [
      nwg-displays
      nemo
      unstable.hyprsunset
      unstable.hyprpolkitagent
      wl-clipboard
      libsForQt5.qt5.qtwayland
    ];

    # xdg.portal = {
    #   enable = true;
    #   # TODO Use pkgs.stdenv.hostPlatform.system instead?
    #   configPackages = [ inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland ];
    #   extraPortals = with pkgs; [
    #     inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland
    #     # xdg-desktop-portal-gtk
    #   ];
    # };

    nixos = {
      programs = {
        xwayland.enable = true;
        uwsm.enable = true;

        hyprland = {
          enable = true;
          withUWSM = true;
          package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
          portalPackage =
            inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
        };
      };
      hardware.graphics = {
        enable = true;
        package =
          inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.mesa.drivers;

        enable32Bit = true;
        package32 =
          inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.pkgsi686Linux.mesa.drivers;
      };
    };

    home.activation.touch = ''
      touch ${config.home.homeDirectory}/.config/hypr/monitors.conf \
            ${config.home.homeDirectory}/.config/hypr/workspaces.conf
    '';

    xdg.configFile = {
      "swaync/config.json".text = builtins.toJSON {scripts = {};};
      "hypr/shaders" = {
        source = ./shaders;
        recursive = true;
      };
    };

    # Log rules: watch -n 0.1 "cat "/tmp/hypr/$(echo $HYPRLAND_INSTANCE_SIGNATURE)/hyprland.log" | grep -v "efresh" | grep "rule" | tail -n 40"
    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = false;
      package = null;
      portalPackage = null;
      plugins = with inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system};
        [borders-plus-plus]
        ++ outputs.lib.optionals (config.modules.hyprland.animations) [
          inputs.hypr-dynamic-cursors.packages.${pkgs.stdenv.hostPlatform.system}.hypr-dynamic-cursors
        ];
      settings = {
        source = [
          "${config.home.homeDirectory}/.config/hypr/monitors.conf"
          "${config.home.homeDirectory}/.config/hypr/workspaces.conf"
        ];
        exec-once = [
          "${pkgs.sway-audio-idle-inhibit}/bin/sway-audio-idle-inhibit"
          "${pkgs.xwaylandvideobridge}/bin/xwaylandvideobridge"
          "systemctl --user start hyprpolkitagent"
          "${hypr-gamemode}"
          # "${./. + /bin/monocle.sh}"
        ];
        plugin = outputs.lib.mkIf (config.modules.hyprland.animations) {
          dynamic-cursors = {
            enabled = true;
            mode = "tilt";
          };
        };
        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 1;
          "col.active_border" = "rgba(717585FF) rgba(707480FF) 90deg";
          "col.inactive_border" = "rgba(616977FF) rgba(636973FF) 90deg";

          # snap = {
          #   enabled = true;
          #   window_gap = 30;
          #   monitor_gap = 30;
          # };

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
        xwayland = {
          force_zero_scaling = true;
        };
        decoration = {
          rounding = 5;

          blur = {
            enabled = true;
            size = 16;
            passes = 3;
            new_optimizations = true;
            ignore_opacity = true;
            vibrancy = 1;
            brightness = 1;
            # xray = true;
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
            "blur,wofi"
            "blur,launcher"
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
        dwindle = {
          pseudotile = true;
          force_split = 2;
        };
        master = {
          new_status = "slave";
          allow_small_split = true;
          smart_resizing = false;
        };
        misc = {
          disable_hyprland_logo = true;
          enable_swallow = true;
          swallow_regex = "^(Alacritty|kitty|org.wezfurlong.wezterm)$";
          animate_manual_resizes = true;
          animate_mouse_windowdragging = true;
        };
        binds = {
          allow_workspace_cycles = true;
        };
        bind = [
          "SUPER, Return, exec, uwsm app -- ${config.modules.${config.defaultTerminal}.exec {}}"
          "SUPER, Q, killactive"
          "SUPER SHIFT, E, exit"
          "SUPER, E, exec, uwsm app -- ${pkgs.nemo}/bin/nemo"
          "SUPER SHIFT, F, togglefloating"
          "SUPER ALT, F, workspaceopt, allfloat"
          "SUPER, F, fullscreen, 0"
          "SUPER, space, fullscreen, 1"
          ''SUPER, D, exec, ${config.programs.fuzzel.package}/bin/fuzzel --launch-prefix="uwsm app -- "''
          "SUPER, A, exec, ${./. + /bin/hypr-pin}"
          ''ALT, P, exec, ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.imagemagick}/bin/convert - -shave 1x1 PNG:- | ${pkgs.wl-clipboard}/bin/wl-copy''
          ''ALT SHIFT, P, exec, ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.imagemagick}/bin/convert - -shave 1x1 PNG:- | ${pkgs.swappy}/bin/swappy -f -''
          # ''ALT CTRL, P, exec, (${pkgs.killall}/bin/killall -SIGINT wf-recorder && (${pkgs.wl-clipboard}/bin/wl-copy < /tmp/screenrecord.mp4; ${pkgs.nemo}/bin/nemo /tmp/screenrecord.mp4)) || (set -euo pipefail; GEOMETRY="$(${pkgs.slurp}/bin/slurp)" && ${pkgs.wf-recorder}/bin/wf-recorder -f /tmp/screenrecord.mp4 -y -g "''${GEOMETRY}")''
          # ''ALT CTRL, P, exec, (${pkgs.killall}/bin/killall -SIGINT wf-recorder && (${pkgs.wl-clipboard}/bin/wl-copy < /tmp/screenrecord.mp4; ${pkgs.nemo}/bin/nemo /tmp/screenrecord.mp4)) || (set -euo pipefail; GEOMETRY="$(${pkgs.slurp}/bin/slurp)" && ${pkgs.wf-recorder}/bin/wf-recorder -f /tmp/screenrecord.mp4 -y -a -g "''${GEOMETRY}")''

          ''SUPER SHIFT, S, exec, hyprctl keyword decoration:screen_shader "${config.home.homeDirectory}/.config/hypr/shaders/$(find "${config.home.homeDirectory}/.config/hypr/shaders" -name *.frag | xargs -n1 basename | fuzzel --dmenu)"''
          "SUPER CTRL SHIFT, S, exec, hyprctl keyword decoration:screen_shader '[[EMPTY]]'"

          "SUPER, left, exec, ${./. + /bin/movefocus.sh} l"
          "SUPER, right, exec, ${./. + /bin/movefocus.sh} r"
          "SUPER, up, exec, ${./. + /bin/movefocus.sh} u"
          "SUPER, down, exec, ${./. + /bin/movefocus.sh} d"

          "SUPER SHIFT, TAB, centerwindow"
          "SUPER SHIFT, Return, layoutmsg, swapwithmaster"
          "SUPER SHIFT, space, layoutmsg, orientationcycle left center"
          "SUPER, bar, layoutmsg, orientationcycle left right"
          "SUPER, bar, layoutmsg, swapsplit"
          "SUPER, O, pseudo"
          "SUPER, B, exec, hyprctl keyword general:layout master"
          "SUPER SHIFT, B, exec, hyprctl keyword general:layout dwindle"

          "SUPER, X, exec, ${lock}"
          "SUPER, L, exec, ${lock}"
          "SUPER, TAB, workspace, previous"

          "SUPER SHIFT, R, exec, hyprctl reload"
          "SUPER, C, exec, ${pkgs.hyprpicker}/bin/hyprpicker -a | tr -d '\\n' | ${pkgs.wl-clipboard}/bin/wl-copy"

          "SUPER SHIFT, C, exec, ${hypr-gamemode} toggle"

          "SUPER, mouse_down, workspace, e+1"
          "SUPER, mouse_up, workspace, e-1"
        ];
        binde = [
          "SUPER CTRL, right, resizeactive, 50 0"
          "SUPER CTRL, left, resizeactive, -50 0"
          "SUPER CTRL, up, resizeactive, 0 -50"
          "SUPER CTRL, down, resizeactive, 0 50"

          "SUPER SHIFT, right, movewindow, r"
          "SUPER SHIFT, left, movewindow, l"
          "SUPER SHIFT, up, movewindow, u"
          "SUPER SHIFT, down, movewindow, d"

          ", XF86AudioRaiseVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume -l 2.0 @DEFAULT_SINK@ 5%+"
          ", XF86AudioLowerVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume -l 2.0 @DEFAULT_SINK@ 5%-"
          ", XF86AudioMute, exec, ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_SINK@ toggle"
          ", XF86AudioMicMute, exec, ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_SOURCE@ toggle"
          ", XF86AudioPause, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
          ", XF86AudioPlay, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
          "SHIFT, XF86AudioMute, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
          ", XF86AudioNext, exec, ${pkgs.playerctl}/bin/playerctl next"
          ", XF86AudioPrev, exec, ${pkgs.playerctl}/bin/playerctl previous"
          ", XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 5%+"
          ", XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 5%-"
        ];
        bindr = [
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
        bindm = [
          "SUPER, mouse:272, movewindow"
          "SUPER SHIFT, mouse:272, movewindow"
          "SUPER, mouse:273, resizewindow"
          "SUPER SHIFT, mouse:273, resizewindow"
        ];
        windowrulev2 = [
          "opacity 0.0 override, class:^(xwaylandvideobridge)$"
          "noanim, class:^(xwaylandvideobridge)$"
          "noinitialfocus, class:^(xwaylandvideobridge)$"
          "maxsize 1 1, class:^(xwaylandvideobridge)$"
          "noblur, class:^(xwaylandvideobridge)$"
          "nofocus, class:^(xwaylandvideobridge)$"

          "pin, class:(pinentry-)(.*)"
          "stayfocused, class:(pinentry-)(.*)"
          "pin, class:(gcr-prompter)"
          "stayfocused, class:(gcr-prompter)"

          "float, class:(.*)(scratchpad)"
          "workspace special silent, class:(.*)(scratchpad)"
          "size 60% 65%, class:(.*)(scratchpad)"
          "center, class:(.*)(scratchpad)"
          "opacity 0.8 0.8, class:(.*)(scratchpad)"

          "noborder, fullscreen:1"

          "opacity 0.8 0.8, class:kitty"
          "opacity 0.8 0.8, class:org.wezfurlong.wezterm"

          "noinitialfocus,class:^jetbrains-(?!toolbox),floating:1"

          # Games
          "noinitialfocus, class:steam"
          "stayfocused, title:^()$,class:steam"
          "minsize 1 1, title:^()$,class:steam"
          "workspace 4 silent, class:steam"
          "workspace 4 silent, class:steamwebhelper"
          "workspace 10, class:osu!"
          "fullscreen, class:steam_app\\d+"
          "monitor 1, class:steam_app_\\d+"
          "workspace 10, class:steam_app_\\d+"

          "workspace 2 silent, class:(discord)"
          "workspace 2 silent, class:(vesktop)"

          "float, class:(firefox)(.*), title:(Picture-in-Picture)"
          "workspace 2, class:(firefox)(.*), title:(Picture-in-Picture)"
          "dimaround, class:(firefox)(.*), title:(Picture-in-Picture)"
          "keepaspectratio, class:(firefox)(.*), title:(Picture-in-Picture)"
          "float, class:(firefox).*, title:(Opening)(.*)"
          "float, class:(firefox).*, title:(Save As)(.*)"

          "float, class:zen, title:(Picture-in-Picture)"
          "workspace 2, class:zen, title:(Picture-in-Picture)"
          "dimaround, class:zen, title:(Picture-in-Picture)"
          "keepaspectratio, class:zen, title:(Picture-in-Picture)"
          "float, class:zen, title:(Opening)(.*)"
          "float, class:zen, title:(Save As)(.*)"

          # Discord has initialClass ' - Discord'
          # Discord popout has initialClass 'Discord Popout'
          # "float, class:(discord), title:^((?! - Discord).)*$"
          # "pin, class:(discord), title:^((?! - Discord).)*$"
          # "noborder, class:(discord), title:^((?! - Discord).)*$"
          # "size 565 317, class:(discord), title:^((?! - Discord).)*$"
          # "move onscreen 100%-0, class:discord, title:^((?! - Discord).)*$"
        ];
      };
      extraConfig = ''
        # Passthrough mode (e.g. for VNC)
        bind=SUPER SHIFT,P,submap,passthrough
        submap=passthrough
        bind=SUPER SHIFT,P,submap,reset
        submap=reset

        # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
        ${builtins.concatStringsSep "\n" (
          builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in ''
              bind = SUPER, ${ws}, exec, hyprctl dispatch focusworkspaceoncurrentmonitor ${toString (x + 1)}
              bind = SUPER SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
            ''
          )
          10
        )}
      '';
    };
  }
