{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.boiler = let
    hostFeatures = lib'.useFeatures self [
      "easyeffects"
      "steam"
      "controllers"
      "ffxiv"
      {
        minecraft = {
          launcher = "prism";
        };
      }
      {
        modding = {
          games = ["unity" "celeste" "outer wilds"];
        };
      }
      {
        emulation = {
          consoles = ["retroarch"];
        };
      }
      "obs"
      "godot"
      {
        android = {
          ide = true;
        };
      }
    ];
  in
    lib'.mkHost {
      inherit self withSystem;
      configuration = _: {
        imports = with self.nixosModules;
          [
            ./hardware-configuration.nix
            host
            homeManager
            thomas
          ]
          ++ hostFeatures.nixos;

        home-manager.users.thomas.imports = hostFeatures.home;

        features.niri.overrides = {
          outputs = {
            "DP-1" = {
              mode = "2560x1440@143.998";
              position = _: {
                props = {
                  x = 0;
                  y = 0;
                };
              };
              focus-at-startup = _: {};
              hot-corners = {
                off = _: {};
              };
            };
            "HDMI-A-1" = {
              mode = "1920x1080@60.000";
              position = _: {
                props = {
                  x = 2560;
                  y = 300;
                };
              };
              hot-corners = {
                off = _: {};
              };
            };
          };
          workspaces = {
            "gaming" = _: {
              props = _: {
                open-on-output = "DP-1";
              };
            };
          };
        };

        system.stateVersion = "24.05";
        networking.hostName = "boiler";

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
      };
    };
}
