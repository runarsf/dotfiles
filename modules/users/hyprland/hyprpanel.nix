{
  config,
  pkgs,
  outputs,
  ...
}:
# cd ~/.cache/ags/hyprpanel
# diff <(cat options_source.json | jq) <(cat options.json | jq)
# nix repl
# :p builtins.fromJSON (builtins.readFile ./options.json)
# TODO Workspace numbering like this https://preview.redd.it/yabai-made-some-minor-tweaks-but-otherwise-been-rocking-v0-fxcau0rvek0e1.png?width=3548&format=png&auto=webp&s=82d702c8d958a3b2436157970103f4d3e36f13e4
# TODO Add nwg-displays as tile on panel
# TODO Github notifications using https://hyprpanel.com/configuration/custom_modules.html
# TODO Blinking css for low battery, override installPhase and cat new.css >> old.css https://github.com/Jas-SinghFSU/HyprPanel/blob/master/nix/default.nix#L44-L54
let
  inherit (outputs.lib) getExe getExe';

  background = "#01010c";
  text = "#f8f8ff";
  dimtext = "#565B66";
  feinttext = "#131721";
  cards = "#0C0F15";
  borderColor = "#434343";

  icons = [
    [
      "class:dev.zed.Zed"
      ""
      "Zed"
    ]
    [
      "class:cursor"
      "󰨞"
      "Code"
    ]
    [
      "class:Emulator"
      ""
      "Android Emulator"
    ]
    [
      "class:jetbrains-studio"
      "󰀴"
      "Android Studio"
    ]
    [
      # FIXME This just makes the icon invisible
      "class:xwaylandvideobridge"
      ""
      ""
    ]
  ];
in
  outputs.lib.mkDesktopModule config "hyprpanel" {
    options' = {
      matugen = outputs.lib.mkEnableOption "Enable matugen";
    };

    config = {
      home.packages = with pkgs; [
        matugen
      ];
      programs.pywal.enable = true;

      programs.hyprpanel = {
        enable = true;

        settings = outputs.lib.mkMerge [
          {
            hyprpanel.useLazyLoading = true;
            notifications = {
              ignore = ["spotify"];
            };
            bar = {
              autoHide = "fullscreen";
              layouts = let
                layout = {
                  left = [
                    "dashboard"
                    "workspaces"
                    "windowtitle"
                  ];
                  middle = ["media"];
                  right = [
                    "volume"
                    "bluetooth"
                    "hypridle"
                    "hyprsunset"
                    "network"
                    "cpu"
                    "ram"
                    "battery"
                    "clock"
                    "notifications"
                  ];
                };
              in {
                "*" = layout;
                "0" =
                  layout
                  // {
                    left = layout.left ++ ["systray"];
                  };
              };
              launcher = {
                autoDetectIcon = true;
                icon = "";
                rightClick = getExe pkgs.fuzzel;
              };
              battery.hideLabelWhenFull = true;
              bluetooth.rightClick = getExe' pkgs.blueman "blueman-manager";
              clock = {
                format = " %a %d. %b  %H:%M ";
                showIcon = false;
              };
              media = {
                middleClick = getExe pkgs.unstable.easyeffects;
                rightClick = getExe pkgs.pwvucontrol;
                show_active_only = true;
              };
              network.showWifiInfo = true;
              notifications = {
                hideCountWhenZero = true;
                show_total = true;
              };
              volume = {
                middleClick = "${getExe' pkgs.wireplumber "wpctl"} set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
                rightClick = "${getExe' pkgs.wireplumber "wpctl"} set-mute @DEFAULT_AUDIO_SINK@ toggle";
                scrollDown = "${getExe' pkgs.wireplumber "wpctl"} set-volume @DEFAULT_AUDIO_SINK@ 5%-";
                scrollUp = "${getExe' pkgs.wireplumber "wpctl"} set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
              };
              workspaces = {
                applicationIconFallback = "";
                ignored = "-.+";
                monitorSpecific = true;
                numbered_active_indicator = "highlight";
                reverse_scroll = true;
                showApplicationIcons = true;
                showWsIcons = true;
                show_icons = false;
                show_numbered = false;
                workspaceMask = false;
                applicationIconMap = builtins.listToAttrs (
                  map (item: {
                    name = builtins.elemAt item 0;
                    value = builtins.elemAt item 1;
                  })
                  icons
                );
              };
              customModules = let
                btop = config.modules.${config.defaultTerminal}.exec "${getExe pkgs.btop}";
              in {
                cpu.leftClick = btop;
                ram.leftClick = btop;
                hyprsunset.label = false;
                hypridle = {
                  label = false;
                  rightClick = "${getExe pkgs.mpv} --no-audio https://roundrobin3.videostreamingwowza.com/visdeurbel2/visdeurbel2.stream/playlist.m3u8";
                };
              };
              windowtitle.title_map =
                map (
                  item:
                    (item |> builtins.head |> builtins.split ":" |> outputs.lib.lists.last |> outputs.lib.singleton)
                    ++ builtins.tail item
                )
                icons;
            };

            tear = true;
            wallpaper.enable = false;
            terminal = config.modules.${config.defaultTerminal}.exec [];
            notifications = {
              showActionsOnHover = false;
            };

            menus = {
              clock = {
                time = {
                  military = true;
                  hideSeconds = true;
                };
                weather = {
                  enabled = false;
                  unit = "metric";
                };
              };
              dashboard = {
                directories.enabled = false;
                stats.enable_gpu = true;
                powermenu.avatar.image = outputs.lib.optionalAttrs (config.avatar != null) "${config.avatar}";
                shortcuts = {
                  left = {
                    shortcut1.command = "zen";
                    shortcut1.icon = "󰈹";
                    shortcut1.tooltip = "Zen";
                    shortcut4.command = getExe pkgs.fuzzel;
                    shortcut4.tooltip = "Apps";
                  };
                  right = {
                    shortcut1.command = "${getExe pkgs.hyprpicker} -a | tr -d '\\n' | ${getExe' pkgs.wl-clipboard "wl-copy"}";
                    shortcut3.command = ''${getExe pkgs.grim} -g "$(${getExe pkgs.slurp})" - | ${getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${getExe' pkgs.wl-clipboard "wl-copy"}'';
                  };
                };
              };
              transitionTime = 100;
              volume.raiseMaximumVolume = true;
              power.lowBatteryNotification = true;
            };

            theme = {
              font = {
                name = "CaskaydiaCove Nerd Font Propo";
                label = "CaskaydiaCove Nerd Font Propo";
                size = "1.1rem";
              };
              name = "monochrome";
              notification = {
                inherit background borderColor;
              };
              systray.dropdownmenu.divider = borderColor;
              bar = {
                outer_spacing = "0.5rem";
                menus = {
                  inherit background cards text; # dimtext feinttext;
                  border.color = borderColor;
                  monochrome = true;
                  label = text;
                };
                transparent = true;
                floating = true;
                buttons = {
                  inherit background text borderColor;
                  icon = text;
                  enableBorders = true;
                  y_margins = "0";
                  borderSize = "1px";
                  monochrome = true;
                  style = "default";
                  workspaces = {
                    numbered_active_highlight_padding = "0.2em";
                    numbered_active_highlight_border = "0.25em";
                    # fontSize = "1em";
                  };
                  padding_x = "0.7rem";
                  radius = "0.5em";
                  workspaces = {
                    hover = "#AAC7FF";
                    numbered_active_underline_color = "#e4e4fc";
                    active = "#AAC7FF";
                    occupied = "#e4e4fc";
                    available = "#c0bfbc";
                  };
                };
              };
            };
          }
          (outputs.lib.mkIf config.modules.hyprpanel.matugen {
            wallpaper.image = "${config.modules.wallpaper}";
            theme = {
              matugen = true;
              matugen_settings = {
                mode = "dark";
                scheme_type = "expressive";
                variant = "monochrome_1";
                contrast = 1;
              };
            };
          })
        ];
      };

      # wayland.windowManager.hyprland.settings.exec = [
      #   ''pgrep "hyprpanel" || (${getExe pkgs.hyprpanel} q; ${getExe pkgs.hyprpanel})''
      # ];
    };
  }
