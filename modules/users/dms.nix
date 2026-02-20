{
  config,
  inputs,
  outputs,
  pkgs,
  ...
}: let
  inherit (outputs.lib) run;
in
  {
    imports = [
      inputs.dms.homeModules.dank-material-shell
      inputs.dms-plugin-registry.modules.default
    ];
  }
  // outputs.lib.mkDesktopModule config "dms" {
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
        launchPrefix = run "";
        syncModeWithPortal = false;
        runDmsMatugenTemplates = false;
        lockScreenNotificationMode = 1;
        notificationTimeoutCritical = 600000;
        notificationCompactMode = true;
        osdPosition = 7;
        osdPowerProfileEnabled = true;
        updaterHideWidget = true;
        screenPreferences = {
          wallpaper = [];
        };
        barConfigs = [
          {
            id = "default";
            name = "Main Bar";
            enabled = true;
            position = 0;
            screenPreferences = [
              "all"
            ];
            showOnLastDisplay = true;
            leftWidgets = [
              {
                id = "launcherButton";
                # enabled = true;
              }
              {
                id = "workspaceSwitcher";
                # enabled = true;
              }
              {
                id = "spacer";
                # enabled = true;
                size = 20;
              }
              {
                id = "focusedWindow";
                # enabled = true;
                focusedWindowCompactMode = false;
              }
              {
                id = "clipboard";
                # enabled = true;
              }
            ];
            centerWidgets = [
              {
                id = "music";
                # enabled = true;
              }
            ];
            rightWidgets = [
              {
                id = "systemTray";
                # enabled = true;
              }
              {
                id = "spacer";
                # enabled = true;
                size = 20;
              }
              {
                id = "cpuUsage";
                # enabled = true;
              }
              {
                id = "memUsage";
                # enabled = true;
              }
              {
                id = "battery";
                # enabled = true;
              }
              {
                id = "controlCenterButton";
                # enabled = true;
              }
              {
                id = "spacer";
                # enabled = true;
                size = 20;
              }
              {
                id = "clock";
                # enabled = true;
              }
              {
                id = "notificationButton";
                # enabled = true;
              }
            ];
            transparency = 0;
            widgetOutlineEnabled = true;
            widgetOutlineColor = "secondary";
            widgetOutlineOpacity = 0.3;
          }
        ];
      };

      managePluginSettings = true;
      plugins = outputs.lib.enable [
        "easyEffects"
        "dankGifSearch"
        "dankBatteryAlerts"
        "dankStickerSearch"
        "gitmojiLauncher"
        "nixMonitor"
      ];
    };
  }
