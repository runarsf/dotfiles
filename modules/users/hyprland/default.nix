{ config, inputs, pkgs, name, system, ... }:

# TODO https://github.com/CMurtagh-LGTM/grab-workspace
# TODO https://github.com/zakk4223/hyprRiver

let
  lock = "${pkgs.hyprlock}/bin/hyprlock";

in {
  imports = [
    inputs.hyprland.homeManagerModules.default
    ./hyprlock.nix
    ./hypridle.nix
    ../wayland.nix
    ../waybar
  ];

  # nixos = {
  #   users.users."${name}".extraGroups = [ "video" ];
  #   services.greetd = {
  #     settings = {
  #       default_session = {
  #         # command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --remember-session";
  #         user = name;
  #       };
  #     };
  #   };
  # };

  nixos.xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-hyprland ]; # xdg-desktop-portal-gtk ];
  xdg.portal.configPackages = with pkgs; [ xdg-desktop-portal-hyprland ]; # xdg-desktop-portal-gtk ];

  nixos.programs.hyprland.enable = true;

  # nixos.nix.settings = {
  #   extra-substituters = [ "https://hyprland.cachix.org" ];
  #   extra-trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
  #   extra-trusted-users = [ name ];
  # };

  home.packages = with pkgs; [
    unstable.pyprland
    master.nwg-displays
    master.nwg-look
    wlr-randr
    swaynotificationcenter
    gtk3
    polkit_gnome
    libnotify
    brightnessctl
    xwaylandvideobridge
    xwayland
    wofi
    hyprcursor
    pulseaudio
    pipewire
    wireplumber
    slurp
    waypipe
    cinnamon.nemo
    wl-clipboard
    wl-clip-persist
    libsForQt5.qt5.qtwayland
    qt6.qtwayland
    wev
    swww
    unstable.waypaper
    jq
    sway-audio-idle-inhibit
    gnome.seahorse
    kanshi

    (pkgs.stdenv.mkDerivation {
      name = "hyprshot";
      src = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/runarsf/screenshot/main/bin/hyprshot";
        sha256 = "sha256-nIlSJQ2IwFgRDs5CfMdsjJi8fdDEgrpV8gOCrJ40fmw=";
      };
      unpackPhase = "true";
      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/hyprshot
        chmod +x $out/bin/hyprshot
      '';
    })
    wf-recorder
    grim
    ffmpeg
  ];
  home.sessionVariables = {
    LIBSEAT_BACKEND = "logind";
    XDG_SESSION_TYPE = "wayland";
    WLR_NO_HARDWARE_CURSORS = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    GDK_SCALE = "2";
  };

  home.activation.touch = ''
    touch ${config.home.homeDirectory}/.config/hypr/monitors.conf \
          ${config.home.homeDirectory}/.config/hypr/workspaces.conf
  '';

  xdg.configFile."swaync/config.json".text = builtins.toJSON {
    scripts = {};
  };

  # TODO Scratchpad binds:
  #  M-n   - Toggle scratchpad ws containing all scratchpads, spawning zsh-scratch if not running
  #  M-S-n - Spawn zsh-scrath
  #  M-S-p - Spawn xonsh-scratch
  xdg.configFile."hypr/pyprland.json".text = builtins.toJSON {
    pyprland.plugins = [ "scratchpads" ];
    scratchpads = {
      term = {
        command =
          "${config.programs.kitty.package}/bin/kitty --class scratchpad --title scratchpad";
        hide = false;
      };
      math = {
        command =
          "${config.programs.kitty.package}/bin/kitty -o font_size=16 --class math-scratchpad --title math-scratchpad ${pkgs.xonsh}/bin/xonsh";
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
    # package = pkgs.inputs.hyprland.hyprland.override { wrapRuntimeDeps = false; };
    systemd = {
      enable = true;
      variables = ["--all"];
    };
    plugins = with inputs.hyprland-plugins.packages.${system}; [
      borders-plus-plus
    ];
    settings = {
      source = [ "${config.home.homeDirectory}/.config/hypr/monitors.conf" ];
      exec-once = [
        "${pkgs.wl-clip-persist}/bin/wl-clip-persist --clipboard both"
        "${pkgs.pyprland}/bin/pypr"
        "${pkgs.sway-audio-idle-inhibit}/bin/sway-audio-idle-inhibit"
        "${pkgs.swaynotificationcenter}/bin/swaync"
        "${./. + /bin/monocle.sh}"
      ];
      exec = [
        "${pkgs.swww}/bin/swww kill; ${pkgs.swww}/bin/swww query || ${pkgs.swww}/bin/swww init"
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

        sensitivity = "-0.4";
        accel_profile = "flat";

        follow_mouse = 1;
        mouse_refocus = false;

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
          size = 5;
          passes = 2;
        };

        layerrule = [
          "blur,wofi"
        ];

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
        always_center_master = true;
      };
      gestures = { workspace_swipe = false; };
      # "device:epic mouse V1" = { sensitivity = -0.5; };
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
        "${mod}, space, fullscreen, 1"
        "${mod}, D, exec, ${pkgs.wofi}/bin/wofi --show drun"
        "${mod}, A, exec, ${./. + /bin/hypr-pin}"
        "ALT, P, exec, hyprshot capture region --copy"
        "${mod} SHIFT, N, exec, ${pkgs.swaynotificationcenter}/bin/swaync-client -t"

        "${mod} SHIFT, TAB, centerwindow"

        "${mod}, left, exec, ${./. + /bin/movefocus.sh} l"
        "${mod}, right, exec, ${./. + /bin/movefocus.sh} r"
        "${mod}, up, exec, ${./. + /bin/movefocus.sh} u"
        "${mod}, down, exec, ${./. + /bin/movefocus.sh} d"

        "${mod} SHIFT, Return, layoutmsg, swapwithmaster"
        "${mod} SHIFT, space, layoutmsg, orientationcycle left center"
        "${mod}, bar, layoutmsg, orientationcycle left right"

        "${mod}, X, exec, ${lock}"
        # "${mod}, TAB, exec, ${./. + /bin/hypr-ws} previous"
        "${mod}, TAB, workspace, previous"

        "${mod} SHIFT, R, exec, hyprctl reload"
        "${mod}, C, exec, ${pkgs.wl-color-picker}/bin/wl-color-picker"

        "${mod} SHIFT, C, exec, ${./. + /bin/hypr-gamemode}"

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
      bindr = [
        "${mod} CTRL, right, exec, ${./. + /bin/hypr-snap}"
        "${mod} CTRL, left, exec, ${./. + /bin/hypr-snap}"
        "${mod} CTRL, up, exec, ${./. + /bin/hypr-snap}"
        "${mod} CTRL, down, exec, ${./. + /bin/hypr-snap}"

        "${mod} SHIFT, right, exec, ${./. + /bin/hypr-snap}"
        "${mod} SHIFT, left, exec, ${./. + /bin/hypr-snap}"
        "${mod} SHIFT, up, exec, ${./. + /bin/hypr-snap}"
        "${mod} SHIFT, down, exec, ${./. + /bin/hypr-snap}"

        "${mod}, mouse:272, exec, ${./. + /bin/hypr-snap}"
        "${mod}, mouse:273, exec, ${./. + /bin/hypr-snap}"
      ];
      bindm = [ "${mod}, mouse:272, movewindow" "${mod}, mouse:273, resizewindow" ];
      windowrulev2 = [
        "opacity 0.0 override 0.0 override,class:^(xwaylandvideobridge)$"
        "noanim,class:^(xwaylandvideobridge)$"
        "nofocus,class:^(xwaylandvideobridge)$"
        "noinitialfocus,class:^(xwaylandvideobridge)$"

        "float, title:^(oneko)$"
        "noblur, title:^(oneko)$"
        "nofocus, title:^(oneko)$"
        "noshadow, title:^(oneko)$"
        "noborder, title:^(oneko)$"

        "float, class:^(.*)(scratchpad)$"
        "workspace special silent, class:^(.*)(scratchpad)$"
        "size 60% 65%, class:^(scratchpad)$"
        "size 50% 55%, class:^(math-scratchpad)$"
        "center, class:^(.*)(scratchpad)$"

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
          bind = ${mod}, ${ws}, exec, hyprctl dispatch focusworkspaceoncurrentmonitor ${toString (x + 1)}
          bind = ${mod} SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
        '') 10)}
    '';
  };
}
