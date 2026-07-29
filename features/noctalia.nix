# TODO: Add `spawn-at-startup "noctalia-shell"` to niri config, but only if noctalia is enabled
{self, inputs, ...}: {
  flake.nixosModules.noctalia = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      self.packages.${pkgs.stdenv.hostPlatform.system}.noctalia
    ];
  };

  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkForce;
  in {
    packages.noctalia = inputs.wrapper-modules.wrappers.noctalia-shell.wrap {
      inherit pkgs;

      settings = {
        wallpaper.enabled = false;
        dock.enabled = false;
        bar = {
          barType = "floating";
          backgroundOpacity = mkForce 0;
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
          # avatarImage = lib.optionalString (config.avatar != null) "${config.avatar}";
          enableShadows = false;
        };
        ui = {
          # fontDefault = outputs.lib.mkForce "CaskaydiaMono NF";
          # fontFixed = outputs.lib.mkForce "CaskaydiaMono NF";
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
  };
}
