{ pkgs, ... }:

{
  home.packages = with pkgs; [
    upower
    playerctl
    gobject-introspection
  ];

  programs.waybar = {
    enable = true;
    package = pkgs.unstable.waybar;
    systemd.enable = true;
    style = ''
      ${builtins.readFile ./style.css}
    '';
    settings = {
      mainBar = {
        height = 30;
        layer = "top";
        position = "top";
        margin = "0 0 0 0";

        modules-left = [
          "hyprland/workspaces"
          "tray"
        ];
        modules-center = [
          "hyprland/window"
        ];
        modules-right = [
          "custom/music"
          "idle_inhibitor"
          "cpu"
          "memory"
          "backlight"
          "pulseaudio"
          "network"
          "battery"
          "clock"
        ];

        "hyprland/workspaces" = {
          "format" = "{icon}";
          "format-window-separator" = " ";
          "window-rewrite-default" = "";
          "window-rewrite" = {
            "title<.*youtube.*>" = " ";
            "class<firefox>" = " ";
            "class<firefox> title<.*github.*>" = " ";
            "kitty" = " ";
            "code" = "󰨞 ";
          };
          "persistent-workspaces" = {
            "*" = 5;
          };
          "format-icons" = {
            "active" = "";
            "default" = "";
            "empty" = "";
          };
        };

        "hyprland/window" = {
          "format" = "{}";
          "max-length" = 50;
          "rewrite" = {
            "(.*) — Mozilla Firefox" = "$1";
          };
          "separate-outputs" = true;
        };

        "custom/pad" = {
          "format" = " ";
          "tooltip" = false;
        };

        "idle_inhibitor" = {
          "format" = "{icon}";
          "format-icons" = {
            "activated" = "";
            "deactivated" = "";
          };
        };

        "tray" = {
          "icon-size" = 14;
          "spacing" = 5;
        };

        "clock" = {
          "tooltip-format" = "<tt><small>{calendar}</small></tt>";
          "calendar" = {
            "mode" = "month";
            "mode-mon-col" = 3;
            "weeks-pos" = "right";
            "on-scroll" = 1;
            "on-click-right" = "mode";
            "format" = {
              "months" = "<span color='#ffead3'><b>{}</b></span>";
              "days" = "<span color='#ecc6d9'><b>{}</b></span>";
              "weeks" = "<span color='#99ffdd'><b>W{}</b></span>";
              "weekdays" = "<span color='#ffcc66'><b>{}</b></span>";
              "today" = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          "actions" = {
            "on-click-right" = "mode";
            "on-click-forward" = "tz_up";
            "on-click-backward" = "tz_down";
            "on-scroll-up" = "shift_up";
            "on-scroll-down" = "shift_down";
          };
          "format" = " {:%H:%M}";
          "format-alt" = "  {:%d/%m/%Y  %H:%M:%S}";
          "interval" = 1;
        };

        "cpu" = {
          "format" = " {usage: >3}%";
          "on-click" = "kitty btop";
        };
        "memory" = {
          "format" = "󰍛 {: >3}%";
          "on-click" = "kitty btop";
        };

        "backlight" = {
          "format" = "{icon} {percent: >3}%";
          "format-icons" = ["" ""];
        };
        "battery" = {
          "states" = {
            "good" = 95;
            "warning" = 30;
            "critical" = 15;
          };
          "format" = "{icon} {capacity: >3}%";
          "format-good" = "";
          "format-full" = "";
          "format-icons" = [" " " " " " " " " "];
        };
        "network" = {
          "format" = "⚠  Disabled";
          "format-wifi" = "  {essid}";
          # "format-ethernet" = " {ifname}: {ipaddr}/{cidr}";
          "format-ethernet" = "  Wired";
          "format-disconnected" = "⚠  Disconnected";
          "on-click" = "nm-connection-editor";
        };
        "pulseaudio" = {
          "scroll-step" = 1;
          "format" = "{icon} {volume: >3}%";
          "format-bluetooth" = "{icon} {volume: >3}%";
          "format-muted" = " muted";
          "format-icons" = {
            "headphones" = "";
            "handsfree" = "";
            "headset" = "";
            "phone" = "";
            "portable" = "";
            "car" = "";
            "default" = ["" ""];
          };
          "on-click" = "wpctl set-mute @DEFAULT_SINK@ toggle";
          "on-click-right" = "wpctl set-mute @DEFAULT_SOURCE@ toggle";
          "on-click-middle" = "pavucontrol";
        };
        "custom/music" = {
          "format" = "{icon}";
          "format-alt" = "{icon} {}";
          "format-icons" = {
            "spotify" = " ";
            "firefox" = "󰈹 ";
            "paused" = " ";
            "default" = "󰎆 ";
          };
          "tooltip" = false;
          "return-type" = "json";
          "on-click-middle" = "playerctl play-pause";
          "on-click-right" = "playerctl next";
          "interval" = 10;
          "exec" = ./music.sh;
        };

      };
    };
  };
}
