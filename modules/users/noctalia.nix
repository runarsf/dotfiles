{
  config,
  inputs,
  outputs,
  pkgs,
  ...
}:
{
  imports = [
    inputs.noctalia.homeModules.default
  ];
}
// outputs.lib.mkDesktopModule config "noctalia" {
  programs.noctalia-shell = {
    enable = true;
    systemd.enable = true;
    # https://docs.noctalia.dev/getting-started/nixos/#config-ref
    settings = {
      wallpaper.enabled = false;
      dock.enabled = false;
      bar = {
        barType = "floating";
        backgroundOpacity = outputs.lib.mkForce 0;
        useSeparateOpacity = true;
        floating = true;
        widgets = {
          left = [
            {
              colorizeDistroLogo = false;
              colorizeSystemIcon = "primary";
              enableColorization = true;
              id = "ControlCenter";
              useDistroLogo = true;
            }
            {
              colorizeIcons = false;
              followFocusedScreen = true;
              hideUnoccupied = true;
              id = "Workspace";
              labelMode = "none";
              showApplications = true;
            }
            {id = "Spacer";}
            {id = "SystemMonitor";}
            {
              defaultSettings = {
                hideBackground = false;
                minimumThreshold = 10;
              };
              id = "plugin:catwalk";
            }
          ];
          center = [
            {id = "ActiveWindow";}
            {id = "MediaMini";}
          ];
          right = [
            {id = "Tray";}
            {id = "Volume";}
            {id = "Battery";}
            {id = "Spacer";}
            {id = "Clock";}
            {id = "NotificationHistory";}
          ];
        };
      };
      general = {
        avatarImage = outputs.lib.optionalString (config.avatar != null) "${config.avatar}";
        enableShadows = false;
      };
      ui = {
        fontDefault = outputs.lib.mkForce "CaskaydiaMono NF";
        fontFixed = outputs.lib.mkForce "CaskaydiaMono NF";
        settingsPanelMode = "centered";
        boxBorderEnabled = true;
      };
      location = {
        weatherEnabled = false;
      };
      notifications.respectExpireTimeout = true;
      brightness.enableDdcSupport = true;
      colorSchemes.predefinedScheme = "Ayu";
      calendar = {
        cards = [
          {
            enabled = true;
            id = "calendar-header-card";
          }
          {
            enabled = true;
            id = "calendar-month-card";
          }
          {
            enabled = false;
            id = "weather-card";
          }
        ];
      };
      controlCenter.cards = [
        {
          enabled = true;
          id = "profile-card";
        }
        {
          enabled = true;
          id = "shortcuts-card";
        }
        {
          enabled = true;
          id = "audio-card";
        }
        {
          enabled = true;
          id = "brightness-card";
        }
        {
          enabled = true;
          id = "weather-card";
        }
        {
          enabled = true;
          id = "media-sysmon-card";
        }
      ];
    };
  };
}
