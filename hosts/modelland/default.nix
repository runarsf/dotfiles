{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.modelland = lib'.mkHost {
    inherit self withSystem;
    configuration = {
      pkgs,
      lib,
      ...
    }: let
      inherit (lib) mkForce;

      hostFeatures = lib'.useFeatures self [
        "osu"
        "steam"
        "controllers"
        "easyeffects"
        "go"
        {
          nvidia = {
            branch = "legacy_580";
            open = false;
          };
        }
        {
          zram = {
            swapfile = "/swap/swapfile";
          };
        }
      ];
    in {
      imports = with self.nixosModules;
        [
          ./hardware-configuration.nix
          host
          homeManager
          runar
        ]
        ++ hostFeatures.nixos;

      home-manager.users.runar = {
        imports = hostFeatures.home;

        home.packages = [pkgs.bruno];
      };

      system.stateVersion = "26.05";
      networking.hostName = "modelland";

      time.timeZone = mkForce "Europe/Zurich";

      features = {
        fonts.oled = true;
        niri.overrides.outputs."GIGA-BYTE TECHNOLOGY CO., LTD. GO27Q24G 26112F001094" = {
          mode = "2560x1440@239.901";
          position = _: {
            props = {
              x = 0;
              y = 0;
            };
          };
          "variable-refresh-rate" = _: {
            props."on-demand" = true;
          };
        };
      };

      # services.desktopManager.plasma6.enable = true;
      # services.displayManager.plasma-login-manager.enable = true;
      # services.desktopManager.gnome.enable = true;
      # services.displayManager.gdm.enable = true;

      # Samsung USB-C headset negotiates full-speed only; its duplex profile
      # (mic + playback) exceeds full-speed USB bandwidth for the mic's
      # altsetting, causing a suspend/resume crash loop that took down
      # quickshell. Force output-only so the capture interface never opens.
      services.pipewire.wireplumber.extraConfig."51-usbc-headset-output-only" = {
        "device.profile.priority.rules" = [
          {
            matches = [
              {"device.name" = "alsa_card.usb-Samsung_USBC_Headset_20190816-00";}
            ];
            actions.update-props.priorities = ["output:analog-stereo"];
          }
        ];
      };

      systemd.tmpfiles.rules = [
        "d /games 0755 runar users -"
      ];
    };
  };
}
