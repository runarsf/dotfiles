{
  config,
  pkgs,
  outputs,
  ...
}:

outputs.lib.mkDesktopModule config "waybar" {
  home.packages = with pkgs; [
    upower
    playerctl
    gobject-introspection
    unstable.hyprland-autoname-workspaces
  ];

  programs.waybar = {
    enable = true;
    package = pkgs.unstable.waybar;
    systemd.enable = true;
    style = ''
      ${builtins.readFile ./style.css}
    '';
    settings =
      let
        i = "&#8201;";
      in
      {
        mainBar = {
          height = 30;
          layer = "top";
          position = "top";
          margin = "10 10 0 10";
          reload_style_on_change = true;

          modules-left = [
            "hyprland/workspaces"
            "tray"
            "custom/layout"
          ];
          modules-center = [ ];
          modules-right = [
            "custom/music"
            "custom/notifs"
            "idle_inhibitor"
            "backlight"
            "custom/polycat"
            "cpu"
            "memory"
            "pulseaudio"
            "network"
            "battery"
            "clock"
          ];

          "hyprland/workspaces" = {
            # format = "{icon}";
            format = "{id}";
            format-window-separator = " ";
            # persistent-workspaces."*" = 5;
            format-icons.default = "";
            all-outputs = false;
            # active = "";
            # empty = "";
          };

          idle_inhibitor = {
            format = "{icon}${i}";
            format-icons = {
              activated = "";
              deactivated = "";
            };
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
            format = "${i} {:%H:%M}";
            format-alt = "${i} {:%d/%m/%Y  %H:%M:%S}";
            interval = 1;
          };

          cpu = {
            format = "${i}{usage: >3}%";
            on-click = "${pkgs.kitty}/bin/kitty btop";
          };
          memory = {
            format = "󰧑${i}{: >3}%";
            on-click = "${pkgs.kitty}/bin/kitty btop";
          };

          backlight = {
            format = "{icon}${i}";
            tooltip = "{percent: >3}%";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
            ];
          };
          battery = {
            states = {
              good = 95;
              warning = 30;
              critical = 15;
            };
            format = "{icon}${i}{capacity: >3}%";
            format-full = "";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
            ];
          };
          network = {
            format = "⚠${i} Disabled";
            format-wifi = "${i} {essid}";
            format-ethernet = "${i} {ipaddr}";
            format-disconnected = "⚠${i} Disconnected";
            on-click = "${pkgs.networkmanagerapplet}/bin/nm-connection-editor";
          };
          pulseaudio = {
            scroll-step = 1;
            format = "{icon}${i}{volume: >3}%";
            format-bluetooth = "{icon}${i}{volume: >3}%";
            format-muted = "${i}";
            format-icons = {
              headphones = "";
              handsfree = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [
                ""
                ""
              ];
            };
            on-click = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_SINK@ toggle";
            on-click-right = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_SOURCE@ toggle";
            on-click-middle = "${pkgs.pavucontrol}/bin/pavucontrol";
          };
          "custom/notifs" = {
            format = " ";
            tooltip = false;
            on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t";
          };
          "custom/polycat" = {
            format = "<span font='polycat'>{}</span>";
            tooltip = false;
            exec = outputs.lib.getExe pkgs.polycat;
          };
          "custom/layout" = {
            format = "{}";
            tooltip = false;
            exec = "${./. + /bin/hypr-layout.sh}";
          };
          "custom/music" = {
            format = "{icon}${i}";
            format-alt = "{icon}${i} {}";
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

        };
      };
  };
}
