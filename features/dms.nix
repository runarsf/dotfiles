{inputs, ...}: {
  flake.homeModules.dms = {
    pkgs,
    lib,
    config,
    osConfig,
    ...
  }: let
    inherit (lib) mkIf mkEnableOption optionals;
    inherit (pkgs.stdenv.hostPlatform) system;
    cfg = config.features.dms;
  in {
    imports = [
      inputs.dms.homeModules.dank-material-shell
      inputs.dms-plugin-registry.homeModules.default
    ];

    options.features.dms.aiUsage = mkEnableOption "Claude Code usage widget";

    config = mkIf (pkgs.stdenv.isLinux && (osConfig.host.desktop or true)) {
      home.packages = with pkgs; [
        libinput
      ];

      programs.dank-material-shell = {
        enable = true;
        systemd.enable = true;
        enableSystemMonitoring = true;
        dgop.package = inputs.dgop.packages.${system}.default;
        settings = {
          runUserMatugenTemplates = false;
          widgetBackgroundColor = "s";
          showWorkspaceApps = true;
          workspaceFollowFocus = true;
          showOccupiedWorkspacesOnly = true;
          workspaceOccupiedColorMode = "s";
          workspaceNameIcons = {
            gaming = {
              type = "icon";
              value = "sports_esports";
            };
            chat = {
              type = "icon";
              value = "chat";
            };
            scratch = {
              type = "icon";
              value = "terminal";
            };
          };
          workspaceActiveAppHighlightEnabled = true;
          centeringMode = "geometric";
          weatherEnabled = false;
          launcherLogoMode = "os";
          monoFontFamily = lib.mkDefault "CaskaydiaMono NF";
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
                # { id = "launcherButton"; }
                {id = "workspaceSwitcher";}
                {
                  id = "spacer";
                  size = 20;
                }
                {id = "bongoCat";}
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
              rightWidgets =
                [
                  {id = "systemTray";}
                  {
                    id = "spacer";
                    size = 20;
                  }
                  {id = "catWidget";}
                  {id = "cpuUsage";}
                  {id = "memUsage";}
                ]
                ++ optionals cfg.aiUsage [
                  {id = "claudeCodeUsage";}
                ]
                ++ [
                  {id = "battery";}
                  {
                    id = "spacer";
                    size = 20;
                  }
                  {id = "controlCenterButton";}
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
          claudeCodeUsage.enable = cfg.aiUsage;
          catWidget.enable = true;
          bongoCat = {
            enable = true;
            settings = {
              catYOffset = 2;
              catSizePercent = 80;
            };
          };
        };
      };
    };
  };
}
