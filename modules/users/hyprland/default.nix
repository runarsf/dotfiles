{ config, inputs, outputs, pkgs, system, ... }:

# TODO wallpaper reload script, listen to monitor change
# TODO https://github.com/CMurtagh-LGTM/grab-workspace
# TODO https://github.com/zakk4223/hyprRiver
# TODO https://github.com/zakk4223/hyprWorkspaceLayouts
# TODO https://github.com/ArtsyMacaw/wlogout

let lock = "${pkgs.hyprlock}/bin/hyprlock";

in {
  imports = [
    inputs.hyprland.homeManagerModules.default
    ./hypridle.nix
    ./hyprlock.nix
    ];
    # TODO Move defaultTerminal somewhere else
  } // outputs.lib.mkDesktopModule config "hyprland" {
  modules.wayland.enable = true;
  modules.waybar.enable = true;
  modules.fuzzel.enable = true;
  programs.jq.enable = true;
  services = outputs.lib.enable [ "kanshi" "swaync" ];

  xdg.portal = {
    enable = true;
    config.common.default = "*";
    configPackages = with pkgs; [
      inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
    ];
    extraPortals = with pkgs; [
      inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
    ];
  };

  nixos = {
    programs = {
      xwayland.enable = true;

      hyprland = {
        enable = true;
        package =
          inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
        portalPackage =
          inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
      };
    };
    hardware.graphics = {
      package =
        inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.mesa.drivers;

      enable32Bit = true;
      package32 =
        inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.pkgsi686Linux.mesa.drivers;
    };
  };

  home.packages = with pkgs; [
    master.nwg-displays
    wlr-randr
    polkit_gnome
    libnotify
    brightnessctl
    xwaylandvideobridge
    nemo
    wl-clipboard
    wl-clip-persist
    libsForQt5.qt5.qtwayland
    wev
    swww
    unstable.waypaper
    sway-audio-idle-inhibit
    seahorse
    ffmpeg

    # For clipsync
    (pkgs.clipnotify.overrideAttrs (oldAttrs: {
      version = "master";
      src = pkgs.fetchFromGitHub {
        owner = "cdown";
        repo = "clipnotify";
        rev = "25ba143c7da8ae0f196cb0db2797d30e6d04e15c";
        sha256 = "sha256-m0Ji48cRp4mvhHeNKhXTT4UDK32OUYoMoss/2yc7TDg=";
      };
    }))
    xclip
    wl-clipboard
  ];

  home.activation.touch = ''
    touch ${config.home.homeDirectory}/.config/hypr/monitors.conf \
          ${config.home.homeDirectory}/.config/hypr/workspaces.conf
  '';

  xdg.configFile."swaync/config.json".text = builtins.toJSON { scripts = { }; };

  # TODO Scratchpad binds:
  #  M-n   - Toggle scratchpad ws containing all scratchpads, spawning zsh-scratch if not running
  #  M-S-n - Spawn zsh-scrath
  #  M-S-p - Spawn xonsh-scratch
  xdg.configFile."hypr/pyprland.json".text = builtins.toJSON {
    pyprland.plugins = [ "scratchpads" ];
    scratchpads = {
      term = {
        command = config.modules.${config.defaultTerminal}.exec { };
        lazy = true;
        hide = false;
      };
      math = {
        command = config.modules.${config.defaultTerminal}.exec {
          class = "math-scratchpad";
          command = outputs.lib.getExe pkgs.xonsh;
        };
        lazy = true;
        hide = false;
      };
    };
  };

  xdg.configFile."hypr/shaders" = {
    source = ./shaders;
    recursive = true;
  };

  # Log rules: watch -n 0.1 "cat "/tmp/hypr/$(echo $HYPRLAND_INSTANCE_SIGNATURE)/hyprland.log" | grep -v "efresh" | grep "rule" | tail -n 40"
  wayland.windowManager.hyprland = let mod = "SUPER";
  in {
    enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
    package =
      inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    plugins = with inputs.hyprland-plugins.packages.${system};
      [ borders-plus-plus ];
    settings = {
      source = [ "${config.home.homeDirectory}/.config/hypr/monitors.conf" ];
      exec-once = [
        # "${pkgs.wl-clip-persist}/bin/wl-clip-persist --clipboard both"
        "${pkgs.unstable.pyprland}/bin/pypr"
        "${pkgs.sway-audio-idle-inhibit}/bin/sway-audio-idle-inhibit"
        "${pkgs.swaynotificationcenter}/bin/swaync"
        # "${./. + /bin/clipsync} watch with-notifications"
        "${./. + /bin/monocle.sh}"
      ];
      exec = [
        "${pkgs.swww}/bin/swww kill; ${pkgs.swww}/bin/swww query || ${pkgs.swww}/bin/swww init"
      ];
      general = {
        gaps_in = 5;
        gaps_out = 20;
        border_size = 1;
        "col.active_border" = "rgba(717585FF) rgba(707480FF) 90deg";
        "col.inactive_border" = "rgba(616977FF) rgba(636973FF) 90deg";

        layout = "master";
        resize_on_border = false;
      };
      # hyprctl -j devices | jq -r '.mice | .[] | .name'
      device = outputs.lib.mkDefault [
        {
          name = "logitech-mx-ergo-1";
          sensitivity = "0.6";
        }
        {
          name = "logitech-usb-receiver";
          sensitivity = "-0.4";
        }
      ];
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
      xwayland = { force_zero_scaling = true; };
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
          xray = true;
          noise = 3.0e-2;
          contrast = 1;
        };

        drop_shadow = true;
        shadow_range = 32;
        shadow_render_power = 3;
        shadow_ignore_window = true;
        shadow_scale = 1;
        "col.shadow" = "rgba(00000055)";
        "col.shadow_inactive" = "rgba(00000028)";

        layerrule = [ "blur,wofi" "blur,launcher" ];
      };
      animations = {
        enabled = "yes";

        bezier =
          [ "myBezier, 0.05, 0.9, 0.1, 1.05" "overshot,0.05,0.9,0.1,1.1" ];

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
        always_center_master = true;
      };
      gestures = { workspace_swipe = false; };
      misc = {
        disable_hyprland_logo = true;
        enable_swallow = true;
        swallow_regex = "^(Alacritty|kitty)$";
        animate_manual_resizes = true;
        animate_mouse_windowdragging = true;
      };
      # monitor = [
      #   ", preferred, auto, 1, bitdepth, 10"
      # ];
      binds = { allow_workspace_cycles = true; };
      bind = [
        "${mod}, Return, exec, ${config.modules.${config.defaultTerminal}.exe}"
        "${mod}, Q, killactive"
        "${mod} SHIFT, E, exit"
        "${mod}, E, exec, ${pkgs.nemo}/bin/nemo"
        "${mod}, K, exec, ${pkgs.seahorse}/bin/seahorse"
        "${mod} SHIFT, F, togglefloating"
        "${mod} ALT, F, workspaceopt, allfloat"
        "${mod}, F, fullscreen, 0"
        "${mod}, space, fullscreen, 1"
        "${mod}, D, exec, ${config.programs.fuzzel.package}/bin/fuzzel"
        "${mod}, A, exec, ${./. + /bin/hypr-pin}"
        ''
          ALT, P, exec, ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.imagemagick}/bin/convert - -shave 1x1 PNG:- | ${pkgs.wl-clipboard}/bin/wl-copy''
        ''
          ALT SHIFT, P, exec, ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.imagemagick}/bin/convert - -shave 1x1 PNG:- | ${pkgs.swappy}/bin/swappy -f -''
        ''
          ALT CTRL, P, exec, (${pkgs.killall}/bin/killall -SIGINT wl-screenrec && (${pkgs.wl-clipboard}/bin/wl-copy < /tmp/screenrecord.mp4; ${pkgs.nemo}/bin/nemo /tmp/screenrecord.mp4)) || (cd /tmp; ${pkgs.wl-screenrec}/bin/wl-screenrec -g "$(${pkgs.slurp}/bin/slurp)" --audio)''
        "${mod} SHIFT, N, exec, ${pkgs.swaynotificationcenter}/bin/swaync-client -t"

        ''${mod} SHIFT, S, exec, hyprctl keyword decoration:screen_shader "${config.home.homeDirectory}/.config/hypr/shaders/$(find "${config.home.homeDirectory}/.config/hypr/shaders" -name *.frag | xargs -n1 basename | fuzzel --dmenu)"''
        "${mod} CTRL SHIFT, S, exec, hyprctl keyword decoration:screen_shader '[[EMPTY]]'"
        "${mod}, left, exec, ${./. + /bin/movefocus.sh} l"
        "${mod}, right, exec, ${./. + /bin/movefocus.sh} r"
        "${mod}, up, exec, ${./. + /bin/movefocus.sh} u"
        "${mod}, down, exec, ${./. + /bin/movefocus.sh} d"

        "${mod} SHIFT, TAB, centerwindow"
        "${mod} SHIFT, Return, layoutmsg, swapwithmaster"
        "${mod} SHIFT, space, layoutmsg, orientationcycle left center"
        "${mod}, bar, layoutmsg, orientationcycle left right"
        "${mod}, bar, layoutmsg, swapsplit"
        "${mod}, O, pseudo"
        "${mod}, B, exec, hyprctl keyword general:layout master"
        "${mod} SHIFT, B, exec, hyprctl keyword general:layout dwindle"

        "${mod}, X, exec, ${lock}"
        "${mod}, L, exec, ${lock}"
        # "${mod}, TAB, exec, ${./. + /bin/hypr-ws} previous"
        "${mod}, TAB, workspace, previous"

        "${mod} SHIFT, R, exec, hyprctl reload"
        "${mod}, C, exec, ${pkgs.hyprpicker}/bin/hyprpicker | tr -d '\\n' | ${pkgs.wl-clipboard}/bin/wl-copy"

        "${mod} SHIFT, C, exec, ${./. + /bin/hypr-gamemode}"
        ''
          ${mod}, V, exec, ${pkgs.wl-clipboard}/bin/wl-paste -t text -w bash -c '[ "$(${pkgs.xclip}/bin/xclip -selection clipboard -o)" = "$(${pkgs.wl-clipboard}/bin/wl-paste -n)" ] || [ "$(${pkgs.wl-clipboard}/bin/wl-paste -l | grep image)" = "" ] && ${pkgs.xclip}/bin/xclip -selection clipboard' ''

        "${mod}, mouse_down, workspace, e+1"
        "${mod}, mouse_up, workspace, e-1"

        "${mod}, N, exec, ${pkgs.pyprland}/bin/pypr toggle term"
        "${mod}, P, exec, ${pkgs.pyprland}/bin/pypr toggle math"
      ];
      binde = [
        "${mod} CTRL, right, resizeactive, 50 0"
        "${mod} CTRL, left, resizeactive, -50 0"
        "${mod} CTRL, up, resizeactive, 0 -50"
        "${mod} CTRL, down, resizeactive, 0 50"

        "${mod} SHIFT, right, movewindow, r"
        "${mod} SHIFT, left, movewindow, l"
        "${mod} SHIFT, up, movewindow, u"
        "${mod} SHIFT, down, movewindow, d"

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
      bindr = let
        hypr-snap = pkgs.writers.writePython3 "hypr-snap" {
          flakeIgnore = [ "E305" "E501" "E227" "E302" "E225" ];
        } (builtins.readFile ./bin/hypr-snap.py);
      in [
        "${mod} CTRL, right, exec, ${hypr-snap}"
        "${mod} CTRL, left, exec, ${hypr-snap}"
        "${mod} CTRL, up, exec, ${hypr-snap}"
        "${mod} CTRL, down, exec, ${hypr-snap}"

        "${mod} SHIFT, right, exec, ${hypr-snap}"
        "${mod} SHIFT, left, exec, ${hypr-snap}"
        "${mod} SHIFT, up, exec, ${hypr-snap}"
        "${mod} SHIFT, down, exec, ${hypr-snap}"

        "${mod}, mouse:272, exec, ${hypr-snap}"
        "${mod}, mouse:273, exec, ${hypr-snap}"
      ];
      bindm = [
        "${mod}, mouse:272, movewindow"
        "${mod} SHIFT, mouse:272, movewindow"
        "${mod}, mouse:273, resizewindow"
        "${mod} SHIFT, mouse:273, resizewindow"
      ];
      windowrulev2 = [
        "opacity 0.0 override, class:^(xwaylandvideobridge)$"
        "noanim, class:^(xwaylandvideobridge)$"
        "noinitialfocus, class:^(xwaylandvideobridge)$"
        "maxsize 1 1, class:^(xwaylandvideobridge)$"
        "noblur, class:^(xwaylandvideobridge)$"

        "float, class:^(.*)(scratchpad)$"
        "workspace special silent, class:^(.*)(scratchpad)$"
        "size 60% 65%, class:^(scratchpad)$"
        "size 50% 55%, class:^(math-scratchpad)$"
        "center, class:^(.*)(scratchpad)$"

        "noborder, fullscreen:1"

        "opacity 0.8 0.8, class:^(kitty)$"
        "opacity 0.8 0.8, class:^(scratchpad)$"
        # "opacity 0.8 override,title:^(.*)(New Tab)(.*)$"
        # "opacity 0.8 override,title:^(Mozilla Firefox)(.*)$"
        # "opacity 0.8 override,title:^(🦊 — Mozilla Firefox)$"

        "noinitialfocus,class:^jetbrains-(?!toolbox),floating:1"

        "noinitialfocus, class:^(steam)$"
        "stayfocused, title:^()$,class:^(steam)$"
        "minsize 1 1, title:^()$,class:^(steam)$"

        "workspace 2 silent, class:^(WebCord)$"
        "workspace 2 silent, class:^(Discord)$"
        "workspace 2 silent, class:^(vesktop)$"
        "workspace 4 silent, class:^(steam)$"
        "workspace 4 silent, class:^(steamwebhelper)$"
        "workspace 7 silent, class:^(easyeffects)$"
        "workspace 7, class:^(Carla2)$"
        "workspace 7, class:^(helvum)$"
        "workspace 7, class:^(qpwgraph)$"
        "workspace 10, class:^(osu!)$"
        "workspace 10, class:^(steam_app_)(.*)$"
        "fullscreen, class:^steam_app\\d+$"
        "monitor 1, class:^steam_app_\\d+$"
        "workspace 10, class:^steam_app_\\d+$"

        "float, class:(org.kde.polkit-kde-authentication-agent-1)"
        "float, class:(seahorse)"
        "float, class:^(firefox)(.*)$, title:(Picture-in-Picture)"
        "workspace 2, class:^(firefox)(.*)$, title:(Picture-in-Picture)"
        "dimaround, class:^(firefox)(.*)$, title:(Picture-in-Picture)"
        "keepaspectratio, class:^(firefox)(.*)$, title:(Picture-in-Picture)"

        # Discord has initialClass ' - Discord'
        # Discord popout has initialClass 'Discord Popout'
        "float, class:(discord), title:^((?! - Discord).)*$"
        "pin, class:(discord), title:^((?! - Discord).)*$"
        "noborder, class:(discord), title:^((?! - Discord).)*$"
        "size 565 317, class:(discord), title:^((?! - Discord).)*$"
        "move onscreen 100%-0, class:discord, title:^((?! - Discord).)*$"

        # WebCord has initialClass '[ID] WebCord - #channel'
        # WebCord popout has initialClass 'vc name'
        "float, class:(WebCord), title:^((?!WebCord - ).)*$"
        "pin, class:(WebCord), title:^((?!WebCord - ).)*$"
        "noborder, class:(WebCord), title:^((?!WebCord - ).)*$"
        "size 565 317, class:(WebCord), title:^((?!WebCord - ).)*$"
        "move onscreen 100%-0, class:(WebCord), title:^((?!WebCord - ).)*$"

        "float, class:^(firefox).*$, title:^(Opening)(.*)$"
        "float, class:^(firefox).*$, title:^$"
        "float, class:^(firefox).*$, title:^(Save As)$"
        "float, class:^(blueman-manager)$"
        "float, class:^(pavucontrol)$"

        "pin, class:(gcr-prompter)"
        "stayfocused, class:(gcr-prompter)"
        "stayfocused, class:^(pinentry-)"
      ];
    };
    extraConfig = ''
      # Passthrough mode (e.g. for VNC)
      bind=${mod} SHIFT,P,submap,passthrough
      submap=passthrough
      bind=${mod} SHIFT,P,submap,reset
      submap=reset

      # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
      ${builtins.concatStringsSep "\n" (builtins.genList (x:
        let ws = let c = (x + 1) / 10; in builtins.toString (x + 1 - (c * 10));
        in ''
          bind = ${mod}, ${ws}, exec, hyprctl dispatch focusworkspaceoncurrentmonitor ${
            toString (x + 1)
          }
          bind = ${mod} SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
        '') 10)}
    '';
  };
}
