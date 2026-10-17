{
  self,
  withSystem,
  lib',
  ...
}: let
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
    "randomizer"
    "obs"
    "godot"
    {
      android = {
        ide = true;
      };
    }
  ];
in {
  flake.nixosConfigurations.boiler = lib'.mkHost {
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

      networking.hostName = "boiler";
      system.stateVersion = "24.05";

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

      home-manager.users.thomas.imports = hostFeatures.home;
    };
  };
}
