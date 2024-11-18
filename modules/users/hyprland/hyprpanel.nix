{ config, pkgs, inputs, outputs, ... }:

# cd ~/.cache/ags/hyprpanel

# diff <(cat options_source.json | jq) <(cat options.json | jq)

# nix repl
# :p builtins.fromJSON (builtins.readFile ./options.json)

let
  background = "#01010c";
  card = "#0C0F15";
  border = "#434343";
  options = builtins.toJSON {
    "bar.battery.hideLabelWhenFull" = true;
    "bar.bluetooth.rightClick" = "${pkgs.blueman}/bin/blueman-manager";
    "bar.clock.format" = " %a %d. %b  %H:%M ";
    "bar.clock.showIcon" = false;
    "bar.customModules.cpu.leftClick" = "${pkgs.btop}/bin/btop";
    "bar.customModules.hyprsunset.label" = false;
    "bar.customModules.ram.leftClick" = "${pkgs.btop}/bin/btop";
    "bar.launcher.autoDetectIcon" = true;
    "bar.launcher.icon" = "";
    "bar.launcher.rightClick" = "${pkgs.fuzzel}/bin/fuzzel";
    "bar.layouts" = {
      "*" = {
        left = [ "dashboard" "workspaces" "windowtitle" "systray" ];
        middle = [ "media" ];
        right = [
          "volume"
          "hyprsunset"
          "network"
          "bluetooth"
          "cpu"
          "ram"
          "battery"
          "clock"
          "notifications"
        ];
      };
    };
    "bar.media.middleClick" = "${pkgs.unstable.easyeffects}/bin/easyeffects";
    "bar.media.rightClick" = "${pkgs.pwvucontrol}/bin/pwvucontrol";
    "bar.media.show_active_only" = true;
    "bar.network.showWifiInfo" = true;
    "bar.notifications.hideCountWhenZero" = true;
    "bar.notifications.show_total" = true;
    "bar.volume.middleClick" =
      "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
    "bar.volume.rightClick" =
      "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
    "bar.volume.scrollDown" =
      "${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
    "bar.volume.scrollUp" =
      "${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
    "bar.windowtitle.custom_title" = false;
    "bar.workspaces.applicationIconFallback" = "";
    "bar.workspaces.hideUnoccupied" = false;
    "bar.workspaces.ignored" = "-.+";
    "bar.workspaces.monitorSpecific" = true;
    "bar.workspaces.numbered_active_indicator" = "highlight";
    "bar.workspaces.reverse_scroll" = true;
    "bar.workspaces.showApplicationIcons" = true;
    "bar.workspaces.showWsIcons" = true;
    "bar.workspaces.show_icons" = false;
    "bar.workspaces.show_numbered" = false;
    "bar.workspaces.workspaceMask" = false;
    "hyprpanel.restartCommand" =
      "${pkgs.hyprpanel}/bin/hyprpanel -q; ${pkgs.hyprpanel}/bin/hyprpanel";
    "menus.bluetooth.showBattery" = true;
    "menus.clock.time.military" = true;
    "menus.clock.weather.enabled" = false;
    "menus.dashboard.directories.enabled" = false;
    "menus.dashboard.powermenu.avatar.image" =
      outputs.lib.optionalAttrs (config.avatar != null) config.avatar;
    "menus.dashboard.shortcuts.left.shortcut1.command" = "zen";
    "menus.dashboard.shortcuts.left.shortcut1.icon" = "󰈹";
    "menus.dashboard.shortcuts.left.shortcut1.tooltip" = "Zen";
    "menus.dashboard.shortcuts.left.shortcut4.command" =
      "${pkgs.fuzzel}/bin/fuzzel";
    "menus.dashboard.shortcuts.left.shortcut4.tooltip" = "Apps";
    "menus.dashboard.shortcuts.right.shortcut1.command" =
      "${pkgs.hyprpicker}/bin/hyprpicker -a | tr -d '\\n' | ${pkgs.wl-clipboard}/bin/wl-copy";
    "menus.dashboard.shortcuts.right.shortcut3.command" = ''
      ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.imagemagick}/bin/convert - -shave 1x1 PNG:- | ${pkgs.wl-clipboard}/bin/wl-copy'';
    "menus.transitionTime" = 100;
    "menus.volume.raiseMaximumVolume" = true;
    "notifications.showActionsOnHover" = false;
    tear = true;
    terminal = config.modules.${config.defaultTerminal}.exec { };
    "theme.bar.floating" = true;
    "theme.bar.buttons.borderSize" = "1px";
    "theme.bar.buttons.enableBorders" = true;
    "theme.bar.buttons.monochrome" = true;
    "theme.bar.buttons.style" = "default";
    "theme.bar.transparent" = true;
    "theme.font.name" = "CaskaydiaCove Nerd Font";
    "theme.font.size" = "1rem";
    "theme.bar.buttons.background" = background;
    "theme.bar.buttons.borderColor" = border;
    "theme.bar.buttons.text" = "#f8f8ff";
    "theme.bar.buttons.icon" = "#f8f8ff";
    "theme.bar.buttons.workspaces.hover" = "#7cb5e0";
    "theme.bar.buttons.workspaces.numbered_active_underline_color" = "#e4e4fc";
    "theme.bar.buttons.workspaces.active" = "#a7c4d9";
    "theme.bar.buttons.workspaces.occupied" = "#e4e4fc";
    "theme.bar.buttons.workspaces.available" = "#c0bfbc";
    "theme.bar.buttons.workspaces.numbered_active_highlight_padding" = "0.25em";
    "theme.bar.buttons.workspaces.numbered_active_highlight_border" = "0.25em";
    "theme.bar.buttons.workspaces.fontSize" = "1em";
    "theme.bar.buttons.padding_x" = "0.7rem";
    "theme.bar.buttons.radius" = "0.5em";
    "theme.bar.outer_spacing" = "0";
    "theme.bar.buttons.y_margins" = "0";
    "theme.bar.border_radius" = "0.4em";
    "theme.bar.margin_top" = "0.5em";
    "theme.bar.menus.background" = background;
    "theme.bar.menus.cards" = background;
    "theme.bar.menus.menu.bluetooth.background.color" = background;
    "theme.bar.menus.menu.bluetooth.card.color" = card;
    "theme.bar.menus.menu.bluetooth.border.color" = border;
    "theme.bar.menus.menu.volume.border.color" = border;
    "theme.bar.menus.menu.volume.card.color" = card;
    "theme.bar.menus.menu.volume.background.color" = background;
    "theme.bar.menus.menu.network.card.color" = card;
    "theme.bar.menus.menu.network.background.color" = background;
    "theme.bar.menus.menu.network.border.color" = border;
    "theme.bar.menus.menu.battery.border.color" = border;
    "theme.bar.menus.menu.battery.card.color" = card;
    "theme.bar.menus.menu.battery.background.color" = background;
    "theme.bar.menus.menu.clock.background.color" = background;
    "theme.bar.menus.menu.clock.card.color" = card;
    "theme.bar.menus.menu.clock.border.color" = border;
    "theme.notification.background" = background;
    "theme.notification.border" = border;
    "theme.bar.menus.menu.notifications.card" = card;
    "theme.bar.menus.menu.notifications.background" = background;
    "theme.bar.menus.menu.notifications.border" = border;
    "theme.bar.menus.menu.media.background.color" = background;
    "theme.bar.menus.menu.media.border.color" = border;
    "theme.bar.menus.menu.dashboard.border.color" = border;
    "theme.bar.menus.menu.dashboard.background.color" = background;
    "theme.bar.menus.menu.dashboard.card.color" = card;
  };

in outputs.lib.mkDesktopModule config "hyprpanel" {
  nixpkgs.overlays = [ inputs.hyprpanel.overlay ];

  home.packages = with pkgs; [
    hyprpanel
    gpu-screen-recorder
    gpu-screen-recorder-gtk
  ];

  home.file = {
    ".local/cache/ags/hyprpanel/options.json" = {
      target = ".local/cache/ags/hyprpanel/options_source.json";
      onChange =
        "cat ~/.local/cache/ags/hyprpanel/options_source.json > ~/.local/cache/ags/hyprpanel/options.json";
      text = options;
    };
    ".cache/ags/hyprpanel/options.json" = {
      target = ".cache/ags/hyprpanel/options_source.json";
      onChange =
        "cat ~/.cache/ags/hyprpanel/options_source.json > ~/.cache/ags/hyprpanel/options.json";
      text = options;
    };
  };

  wayland.windowManager.hyprland.settings.exec =
    [ "${pkgs.hyprpanel}/bin/hyprpanel -q; ${pkgs.hyprpanel}/bin/hyprpanel" ];
}
