{ config, inputs, pkgs, name, system, ... }:

# TODO https://github.com/CMurtagh-LGTM/grab-workspace

let lock = "${pkgs.hyprlock}/bin/hyprlock";

in {
  imports = [
    ../wayland.nix
  ];

  system = {
    users.users.${name}.extraGroups = [ "video" ];
    services.greetd = {
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --cmd Hyprland";
          user = name;
        };
      };
    };
    xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-hyprland ];
  };

  nix.settings = {
    extra-substituters = [ "https://hyprland.cachix.org" ];
    extra-trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
    extra-trusted-users = [ name ];
  };

  home.packages = with pkgs; [
    pyprland
    master.nwg-displays
    swaynotificationcenter
    gtk3
    polkit_gnome
    libnotify
    brightnessctl
    xwaylandvideobridge
    xdg-desktop-portal-hyprland
    xwayland
    wofi
    pulseaudio
    unstable.hyprlock
    slurp
    waypipe
    cinnamon.nemo
    wl-clipboard
    wl-clip-persist
    libsForQt5.qt5.qtwayland
    qt6.qtwayland
    wev
    swww
    waypaper
    jq
    nwg-look
    sway-audio-idle-inhibit
    swayidle
    gnome.seahorse
    # wl-mirror
    # wl-mirror-pick
    # xdg-utils-spawn-terminal # Patched to open terminal
    # ydotool
    # hyprslurp
    # hyprpicker
    kanshi
    wdisplays
    wf-recorder
    grim
    ffmpeg
    (pkgs.stdenv.mkDerivation {
      name = "hyprshot";
      src = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/runarsf/screenshot/main/bin/hyprshot";
        sha256 = "128mypbbbwsb0f208b07i50srnh7q5dfqd97l0zdl1qgz32cygjy";
      };
      unpackPhase = "true";
      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/hyprshot
        chmod +x $out/bin/hyprshot
      '';
    })
  ];
  home.sessionVariables = {
    LIBSEAT_BACKEND = "logind";
    XDG_SESSION_TYPE = "wayland";
    WLR_NO_HARDWARE_CURSORS = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XCURSOR_SIZE = "24";
    GDK_SCALE = "2";
  };

  home.activation.touch = ''
    touch ${config.home.homeDirectory}/.config/hypr/monitors.conf \
          ${config.home.homeDirectory}/.config/hypr/workspaces.conf
  '';

  xdg.configFile."swaync/config.json".text = builtins.toJSON {
    scripts = {};
  };

  xdg.configFile."hypr/hyprlock.conf".text = ''
    background {
      monitor =
      path = ${./. + /lock.png}
      color = rgba(25, 20, 20, 1.0)

      # all these options are taken from hyprland, see https://wiki.hyprland.org/Configuring/Variables/#blur for explanations
      blur_passes = 4 # 0 disables blurring
      blur_size = 7
      noise = 0.0117
      contrast = 0.8916
      brightness = 0.8172
      vibrancy = 0.1696
      vibrancy_darkness = 0.0
    }

    input-field {
      monitor =
      size = 200, 30
      outline_thickness = 3
      dots_size = 0.33 # Scale of input-field height, 0.2 - 0.8
      dots_spacing = 0.15 # Scale of dots' absolute size, 0.0 - 1.0
      dots_center = false
      outer_color = rgb(151515)
      inner_color = rgb(200, 200, 200)
      font_color = rgb(10, 10, 10)
      fade_on_empty = true
      placeholder_text =
      hide_input = false

      position = 0, -25
      halign = center
      valign = center
    }
    label {
      monitor =
      text = $TIME
      color = rgba(200, 200, 200, 1.0)
      font_size = 80
      font_family = JetBrainsMono NF ExtraBold

      position = 0, 150
      halign = center
      valign = center
    }
    label {
      monitor =
      text = Hi there, $USER
      color = rgba(200, 200, 200, 1.0)
      font_size = 18
      font_family = JetBrains Mono Nerd Font

      position = 0, 10
      halign = center
      valign = center
    }
  '';

  xdg.configFile."hypr/hypridle.conf".text = ''
    general {
      lock_cmd = ${lock}
      before_sleep_cmd = ${lock}
    }
    listener {
      timeout = 300
      on-timeout = ${./. + /bin/hypr-brightness} off
      on-resume = ${./. + /bin/hypr-brightness} on
    }
    listener {
      timeout = 500
      on-timeout = ${lock}
    }
    listener {
      timeout = 900
      on-timeout = systemctl suspend
    }
  '';

  xdg.configFile."hypr/pyprland.json".text = builtins.toJSON {
    pyprland.plugins = [ "scratchpads" ];
    scratchpads = {
      term = {
        command =
          "${config.programs.kitty.package}/bin/kitty --class scratchpad --title scratchpad";
        hide = false;
      };
      qalc = {
        command = "${pkgs.qalculate-gtk}/bin/qalculate-gtk";
        hide = false;
      };
    };
  };

  # Log rules:
  # watch -n 0.1 "cat "/tmp/hypr/$(echo $HYPRLAND_INSTANCE_SIGNATURE)/hyprland.log" | grep -v "efresh" | grep "rule" | tail -n 40"
  wayland.windowManager.hyprland = let
    mod = "SUPER";
  in {
    enable = true;
    package = pkgs.inputs.hyprland.hyprland.override { wrapRuntimeDeps = false; };
    systemd.enable = true;
    plugins = with inputs.hyprland-plugins.packages.${system}; [
      borders-plus-plus
      inputs.hycov.packages.${system}.hycov
    ];
    settings = {
      source = [ "${config.home.homeDirectory}/.config/hypr/monitors.conf" ];
      exec-once = [
        "${pkgs.wl-clip-persist}/bin/wl-clip-persist --clipboard both"
        "${pkgs.pyprland}/bin/pypr"
        "${pkgs.sway-audio-idle-inhibit}/bin/sway-audio-idle-inhibit"
        "${pkgs.swaynotificationcenter}/bin/swaync"
      ];
      general = {
        gaps_in = 5;
        gaps_out = 20;
        border_size = 1;
        "col.active_border" = "rgba(676767ee) rgba(414141ee) 90deg";
        "col.inactive_border" = "rgba(67676766) rgba(41414166) 90deg";

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

        touchpad = {
          natural_scroll = true;
          drag_lock = false;
          tap-and-drag = true;
        };

        sensitivity = "0.6";
      };
      xwayland = { force_zero_scaling = true; };
      decoration = {
        rounding = 5;

        blur = {
          enabled = true;
          size = 3;
          passes = 1;
        };

        drop_shadow = true;
        shadow_range = 14;
        shadow_render_power = 3;
        shadow_ignore_window = true;
        "col.shadow" = "rgba(00000045)";
      };
      animations = {
        enabled = "yes";

        bezier = [ "myBezier, 0.05, 0.9, 0.1, 1.05" "overshot,0.05,0.9,0.1,1.1" ];

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
        new_is_master = true;
        allow_small_split = true;
      };
      gestures = { workspace_swipe = false; };
      "device:epic mouse V1" = { sensitivity = -0.5; };
      misc = {
        disable_hyprland_logo = true;
        enable_swallow = true;
        swallow_regex = "^(Alacritty|kitty)$";
        animate_manual_resizes = true;
        animate_mouse_windowdragging = true;
      };
      binds = { allow_workspace_cycles = true; };
      bind = [
        "${mod}, Return, exec, ${config.programs.kitty.package}/bin/kitty"
        "${mod}, Q, killactive"
        "${mod} SHIFT, E, exit"
        "${mod}, E, exec, ${pkgs.cinnamon.nemo}/bin/nemo"
        "${mod}, K, exec, ${pkgs.gnome.seahorse}/bin/seahorse"
        "${mod} SHIFT, F, togglefloating"
        "${mod} ALT, F, workspaceopt, allfloat"
        "${mod}, F, fullscreen, 0"
        "${mod} SHIFT, space, fullscreen, 1"
        "${mod}, D, exec, ${pkgs.wofi}/bin/wofi --show drun"
        "${mod}, space, exec, ${./. + builtins.toPath "/bin/hypr-layouts"}"
        "${mod}, A, exec, ${./. + builtins.toPath "/bin/hypr-pin"}"
        "ALT, P, exec, hyprshot capture region --copy"
        "${mod} SHIFT, N, exec, ${pkgs.swaynotificationcenter}/bin/swaync-client -t"

        "${mod} SHIFT, TAB, centerwindow"

        "${mod}, left, movefocus, l"
        "${mod}, right, movefocus, r"
        "${mod}, up, movefocus, u"
        "${mod}, down, movefocus, d"

        "${mod}, left, movecursortocorner, 2"
        "${mod}, right, movecursortocorner, 2"
        "${mod}, up, movecursortocorner, 2"
        "${mod}, down, movecursortocorner, 2"

        "${mod} SHIFT, Return, layoutmsg, swapwithmaster"
        "${mod}, bar, layoutmsg, orientationcycle left right"

        "${mod}, X, exec, ${lock}"
        "${mod}, TAB, exec, ${./. + builtins.toPath "/bin/hypr-ws"} previous"

        "${mod} SHIFT, R, exec, hyprctl reload"
        "${mod}, C, exec, ${pkgs.wl-color-picker}/bin/wl-color-picker clipboard"

        "${mod} SHIFT, C, exec, ${./. + builtins.toPath "/bin/hypr-gamemode"}"

        "${mod}, mouse_down, workspace, e+1"
        "${mod}, mouse_up, workspace, e-1"

        "${mod}, N, exec, ${pkgs.pyprland}/bin/pypr toggle term"
        "${mod}, P, exec, ${pkgs.pyprland}/bin/pypr toggle qalc"
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
      bindr = [
        "${mod} CTRL, right, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod} CTRL, left, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod} CTRL, up, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod} CTRL, down, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"

        "${mod} SHIFT, right, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod} SHIFT, left, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod} SHIFT, up, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod} SHIFT, down, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"

        "${mod}, mouse:272, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
        "${mod}, mouse:273, exec, ${./. + builtins.toPath "/bin/hypr-snap"}"
      ];
      bindm = [ "${mod}, mouse:272, movewindow" "${mod}, mouse:273, resizewindow" ];
      windowrulev2 = [
        "opacity 0.0 override 0.0 override,class:^(xwaylandvideobridge)$"
        "noanim,class:^(xwaylandvideobridge)$"
        "nofocus,class:^(xwaylandvideobridge)$"
        "noinitialfocus,class:^(xwaylandvideobridge)$"

        "float, class:^(scratchpad)$"
        "workspace special silent, class:^(scratchpad)$"
        "size 60% 65%, class:^(scratchpad)$"
        "center, class:^(scratchpad)$"

        "float, class:^(qalculate-gtk)$"
        "workspace special silent, class:^(qalculate-gtk)$"
        "size 60% 65%, class:^(qalculate-gtk)$"
        "center, class:^(qalculate-gtk)$"

        "noborder, fullscreen:1"

        "opacity 0.8 override,title:^(.*)(New Tab)(.*)$"
        "opacity 0.8 override,title:^(Firefox)(.*)$"

        "noinitialfocus,class:^jetbrains-(?!toolbox),floating:1"

        "noinitialfocus, class:^(steam)$"
        "stayfocused, title:^()$,class:^(steam)$"
        "minsize 1 1, title:^()$,class:^(steam)$"

        "workspace 2 silent, class:^(WebCord)$"
        "workspace 2 silent, class:^(Discord)$"
        "workspace 4 silent, class:^(steam)$"
        "workspace 4 silent, class:^(steamwebhelper)$"
        "workspace 7 silent, class:^(easyeffects)$"
        "workspace 7, class:^(Carla2)$"
        "workspace 7, class:^(helvum)$"
        "workspace 7, class:^(qpwgraph)$"
        "workspace 10, class:^(osu!)$"
        "workspace 10, class:^(steam_app_)(.*)$"

        "float, class:(org.kde.polkit-kde-authentication-agent-1)"
        "float, class:(seahorse)"
        "float, class:^(firefox)(.*)$, title:(Picture-in-Picture)"
        "pin, class:^(firefox)(.*)$, title:(Picture-in-Picture)"

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
      ];
    };
    extraConfig = ''
      bind = ${mod} CTRL,tab,hycov:toggleoverview
      # bind = ALT,left,hycov:movefocus,l
      # bind = ALT,right,hycov:movefocus,r
      # bind = ALT,up,hycov:movefocus,u
      # bind = ALT,down,hycov:movefocus,d

      plugin {
        hycov {
          overview_gappo = 120 #gaps width from screen
          overview_gappi = 24 #gaps width from clients
          hotarea_size = 0 #hotarea size in bottom left,10x10
          enable_hotarea = 0 # enable mouse cursor hotarea
        }
      }

      # Passthrough mode (e.g. for VNC)
      bind=${mod} SHIFT,P,submap,passthrough
      submap=passthrough
      bind=${mod} SHIFT,P,submap,reset
      submap=reset

      # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
      ${builtins.concatStringsSep "\n" (builtins.genList (x:
        let ws = let c = (x + 1) / 10; in builtins.toString (x + 1 - (c * 10));
        in ''
          bind = ${mod}, ${ws}, exec, ${./. + builtins.toPath "/bin/hypr-ws"} ${toString (x + 1)}
          bind = ${mod} SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
        '') 10)}
    '';
  };
}
