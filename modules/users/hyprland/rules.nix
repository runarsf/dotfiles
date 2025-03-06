_: {
  wayland.windowManager.hyprland.settings.windowrulev2 = [
    # TODO When monocle, remove gaps
    #  https://wiki.hyprland.org/Configuring/Workspace-Rules/#smart-gaps

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

    # "float, class:(.*)(scratchpad)"
    # "workspace special silent, class:(.*)(scratchpad)"
    # "size 60% 65%, class:(.*)(scratchpad)"
    # "center, class:(.*)(scratchpad)"
    # "opacity 0.8 0.8, class:(.*)(scratchpad)"

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

    # Ignore maximize requests from apps
    "suppressevent maximize, class:.*"
    # Fix some dragging issues with XWayland
    "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"
  ];
}
