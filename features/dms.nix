{inputs, ...}: {
  flake.homeModules.dms = {
    pkgs,
    lib,
    ...
  }: {
    imports = [
      inputs.dms.homeModules.dank-material-shell
      inputs.dms-plugin-registry.modules.default
    ];

    config = lib.mkIf pkgs.stdenv.isLinux {
      programs.dank-material-shell = {
        enable = true;
        systemd.enable = true;
        enableSystemMonitoring = true;
        dgop.package = inputs.dgop.packages.${pkgs.stdenv.hostPlatform.system}.default;
        settings = {
          runUserMatugenTemplates = false;
          widgetBackgroundColor = "s";
          showWorkspaceApps = true;
          workspaceFollowFocus = true;
          showOccupiedWorkspacesOnly = true;
          workspaceOccupiedColorMode = "s";
          centeringMode = "geometric";
          weatherEnabled = false;
          launcherLogoMode = "os";
          monoFontFamily = "CaskaydiaMono NF";
          launchPrefix = "uwsm app -- ";
          syncModeWithPortal = false;
          runDmsMatugenTemplates = false;
          lockScreenNotificationMode = 1;
          notificationTimeoutCritical = 600000;
          notificationCompactMode = true;
          osdPosition = 7;
          osdPowerProfileEnabled = true;
          updaterHideWidget = true;
          screenPreferences.wallpaper = [];
          barConfigs = [
            {
              id = "default";
              name = "Main Bar";
              enabled = true;
              position = 0;
              screenPreferences = ["all"];
              showOnLastDisplay = true;
              leftWidgets = [
                {id = "launcherButton";}
                {id = "workspaceSwitcher";}
                {
                  id = "spacer";
                  size = 20;
                }
                {
                  id = "focusedWindow";
                  focusedWindowCompactMode = false;
                }
                {id = "clipboard";}
              ];
              centerWidgets = [
                {id = "music";}
              ];
              rightWidgets = [
                {id = "systemTray";}
                {
                  id = "spacer";
                  size = 20;
                }
                {id = "cpuUsage";}
                {id = "memUsage";}
                {id = "battery";}
                {id = "controlCenterButton";}
                {
                  id = "spacer";
                  size = 20;
                }
                {id = "clock";}
                {id = "notificationButton";}
              ];
              transparency = 0;
              widgetOutlineEnabled = true;
              widgetOutlineColor = "secondary";
              widgetOutlineOpacity = 0.3;
            }
          ];
        };

        managePluginSettings = true;
        plugins = {
          easyEffects.enable = true;
          dankGifSearch.enable = true;
          dankBatteryAlerts.enable = true;
          dankStickerSearch.enable = true;
          gitmojiLauncher.enable = true;
          nixMonitor.enable = true;
        };
      };
    };
  };
}
