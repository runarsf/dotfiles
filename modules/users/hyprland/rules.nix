_: {
  # TODO: Make a utility function that can create multiple rules for one class
  # mkRules ["match:class kitty, match:title kitty" "match:class wezterm"] [
  #   "pin"
  #   "noblur"
  # ]
  wayland.windowManager.hyprland.settings.windowrule = [
    # TODO When monocle, remove gaps (or make obvious monocle is active at least)
    #  https://wiki.hyprland.org/Configuring/Workspace-Rules/#smart-gaps

    "pin on, match:class (pinentry-)(.*)"
    "stay_focused on, match:class (pinentry-)(.*)"
    "pin on, match:class (gcr-prompter)"
    "stay_focused on, match:class (gcr-prompter)"

    # "float, match:class (.*)(scratchpad)"
    # "workspace special silent, match:class (.*)(scratchpad)"
    # "size 60% 65%, match:class (.*)(scratchpad)"
    # "center, match:class (.*)(scratchpad)"
    # "opacity 0.8 0.8, match:class (.*)(scratchpad)"

    "border_size 0, match:fullscreen 1"

    "opacity 0.8 0.8, match:class kitty"
    "opacity 0.8 0.8, match:class org.wezfurlong.wezterm"

    "no_initial_focus on, match:class ^jetbrains-(?!toolbox), match:float 1"

    # "plugin:chromakey, match:class code"

    # Games
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
    # "dimaround, match:class (firefox)(.*), match:title (Picture-in-Picture)"
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

    # Discord has initialClass ' - Discord'
    # Discord popout has initialClass 'Discord Popout'
    # "float, match:class (discord), match:title ^((?! - Discord).)*$"
    # "pin, match:class (discord), match:title ^((?! - Discord).)*$"
    # "noborder, match:class (discord), match:title ^((?! - Discord).)*$"
    # "size 565 317, match:class (discord), match:title ^((?! - Discord).)*$"
    # "move onscreen 100%-0, match:class discord, match:title ^((?! - Discord).)*$"

    # Ignore maximize requests from apps
    "suppress_event maximize, match:class .*"
    # Fix some dragging issues with XWayland
    "no_focus on, match:class ^$, match:title ^$, match:xwayland 1, match:float 1, match:fullscreen 0, match:pin 0"
  ];
}
