{
  config,
  pkgs,
  outputs,
  ...
}:
# env GTK_DEBUG=interactive waybar
# TODO If media widget is empty, don't show the group itself
outputs.lib.mkDesktopModule config "waybar" {
  home.packages = with pkgs; [
    upower
    polycat
    playerctl
    gobject-introspection
    unstable.hyprland-autoname-workspaces
  ];

  programs.waybar = {
    enable = true;
    package = pkgs.unstable.waybar;
    systemd.enable = true;
    style = builtins.readFile ./style.css;
    settings = let
      sep = "&#8201;";
    in {
      mainBar = rec {
        height = 30;
        layer = "top";
        position = "top";
        margin = "10 10 0 10";
        reload_style_on_change = true;

        modules-left = ["custom/logo" "hyprland/workspaces" "tray" "custom/layout"];
        modules-center = ["hyprland/window"];
        modules-right = [
          "group/media"
          "idle_inhibitor"
          "backlight"
          "group/cpu"
          "memory"
          "pulseaudio"
          "network"
          "battery"
          "group/clock"
          "custom/notifs"
        ];

        "hyprland/workspaces" = {
          # format = "{icon}";
          format = "{id}";
          format-window-separator = " ";
          # persistent-workspaces."*" = 5;
          format-icons.default = "";
          all-outputs = false;
          format-icons.active = "";
          format-icons.empty = "";
        };

        "hyprland/window" = {
          format = "{}";
          rewrite = {"(.*) — Mozilla Firefox" = "󰈹 $1";};
          separate-outputs = true;
        };

        idle_inhibitor = {
          format = "{icon} ";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };

        "group/clock" = {
          orientation = "inherit";
          drawer = {
            transition-duration = 500;
            children-class = "child";
            transition-left-to-right = false;
          };
          # TODO seconds on hover
          modules = ["clock#time" "clock#date"];
        };
        clock = {
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "month";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            on-click-right = "mode";
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-click-forward = "tz_up";
            on-click-backward = "tz_down";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
          interval = 1;
        };
        "clock#time" = clock // {format = "${sep} {:%H:%M}";};
        "clock#date" = clock // {format = "${sep} {:%d/%m/%Y}";};

        "group/cpu" = {
          orientation = "inherit";
          modules = ["custom/polycat" "cpu"];
        };
        "custom/polycat" = {
          format = "<span font='polycat 16'>{}</span>";
          tooltip = false;
          exec = outputs.lib.getExe pkgs.polycat;
        };
        cpu = {
          # 
          format = "${sep}{usage: >3}%";
          # FIXME You shouldn't need to provide "start" in this, just btop
          on-click = config.modules.${config.defaultTerminal}.exec {
            command = ["start" "--" "btop"];
          };
        };

        memory = {
          format = "󰧑${sep}{: >3}%";
          on-click = config.modules.${config.defaultTerminal}.exec {
            command = ["start" "--" "btop"];
          };
        };

        backlight = {
          format = "{icon}${sep}";
          tooltip = "{percent: >3}%";
          format-icons = ["" "" "" "" "" "" "" "" "" "" "" "" ""];
        };
        battery = {
          states = {
            good = 95;
            warning = 30;
            critical = 15;
          };
          format = "{icon}${sep}{capacity: >3}%";
          format-full = "";
          format-icons = ["" "" "" "" ""];
        };
        network = {
          format = "⚠${sep} Disabled";
          format-wifi = "${sep} {essid}";
          format-ethernet = "${sep} {ipaddr}";
          format-disconnected = "⚠${sep} Disconnected";
          on-click = "${pkgs.networkmanagerapplet}/bin/nm-connection-editor";
        };
        pulseaudio = {
          scroll-step = 1;
          format = "{icon}${sep}{volume: >3}%";
          format-bluetooth = "{icon}${sep}{volume: >3}%";
          format-muted = "${sep}";
          format-icons = {
            headphones = "";
            handsfree = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" ""];
          };
          on-click = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_SINK@ toggle";
          on-click-right = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_SOURCE@ toggle";
          on-click-middle = "${pkgs.pavucontrol}/bin/pavucontrol";
        };
        "custom/notifs" = {
          format = "${sep}";
          tooltip = false;
          on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t";
        };
        "custom/logo" = {
          # FIXME consistent width for icons (2ch maybe)
          format = "${sep}";
          tooltip = false;
          on-click = "${config.programs.fuzzel.package}/bin/fuzzel";
        };
        # FIXME custom/layout
        "custom/layout" = {
          format = "{}";
          tooltip = false;
          exec = "${./. + /bin/hypr-layout.sh}";
        };

        "group/media" = {
          orientation = "inherit";
          drawer = {
            transition-duration = 500;
            children-class = "child";
            transition-left-to-right = false;
          };
          modules = ["custom/media#icon" "custom/media#info"];
        };
        media = {
          format-icons = {
            spotify = "";
            firefox = "󰈹";
            paused = "";
            default = "󰎆";
          };
          tooltip = false;
          return-type = "json";
          on-click-middle = "${pkgs.playerctl}/bin/playerctl play-pause";
          on-click-right = "${pkgs.playerctl}/bin/playerctl next";
          interval = 10;
          exec = "${./. + /bin/music.sh}";
        };
        "custom/media#icon" = media // {format = "{icon}";};
        "custom/media#info" = media // {format-alt = "{}";};
      };
    };
  };
}
