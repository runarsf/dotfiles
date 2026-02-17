{
  config,
  inputs,
  outputs,
  pkgs,
  ...
}: let
  inherit (outputs.lib) run runOnce toggle getExe';
in
  {
    imports = [
      inputs.vicinae.homeManagerModules.default
    ];
  }
  // outputs.lib.mkDesktopModule config "vicinae" {
    services.vicinae = {
      enable = true;
      systemd = {
        enable = true;
        autoStart = true;
        environment = {
          USE_LAYER_SHELL = 1;
        };
      };
      settings = {
        "$schema" = "https://vicinae.com/schemas/config.json";
        close_on_focus_loss = true;
        consider_preedit = true;
        pop_to_root_on_close = true;
        favicon_service = "twenty";
        search_files_in_root = true;
        # Find IDs from Vicinae > "Show Installed Extensions" > ^B > Copy ID
        providers = {
          # https://github.com/vicinaehq/extensions/blob/main/extensions/awww-switcher/package.json
          "@sovereign/vicinae-extension-awww-switcher-0" = {
            preferences = {
              wallpaperPath = "${config.home.homeDirectory}/Pictures/Wallpapers";
              transitionDuration = 1;
            };
          };
        };
      };
      extensions = with inputs.vicinae-extensions.packages.${pkgs.stdenv.hostPlatform.system}; [
        bluetooth
        nix
        power-profile
        awww-switcher
      ];
    };

    home.packages = with pkgs; [
      inputs.awww.packages.${pkgs.stdenv.hostPlatform.system}.awww
      matugen
    ];

    wayland.windowManager.hyprland.settings = {
      bind = [''SUPER, R, exec, ${run "vicinae toggle"}''];
      layerrule = [
        {
          name = "vicinae-blur";
          blur = "on";
          ignore_alpha = 0;
          "match:namespace" = "vicinae";
        }
        {
          name = "vicinae-no-animation";
          no_anim = "on";
          "match:namespace" = "vicinae";
        }
      ];
      exec-once = [
        (run <| getExe' inputs.awww.packages.${pkgs.stdenv.hostPlatform.system}.awww "awww-daemon")
      ];
    };
  }
