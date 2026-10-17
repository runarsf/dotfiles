{
  self,
  withSystem,
  lib',
  ...
}:
{
  flake.nixosConfigurations.modelland = lib'.mkHost {
    inherit self withSystem;

    hostName = "modelland";
    stateVersion = "26.05";
    users = [ "runar" ];

    features = [
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
        zram = {
          swapfile = "/swap/swapfile";
        };
        hyprland = {
          animations = true;
        };
      }
    ];

    imports = [ ./hardware-configuration.nix ];

    configuration =
      {
        pkgs,
        lib,
        ...
      }:
      {
        time.timeZone = lib.mkForce "Europe/Zurich";

        # Windows has its own ESP (sda1), so systemd-boot can't autodetect it.
        # The handle comes from the EDK2 shell: `map -c`, then `ls <handle>:EFI`
        # until one lists Microsoft.
        boot.loader.systemd-boot = {
          edk2-uefi-shell.enable = true;
          # windows."11".efiDeviceHandle = "";
        };

        systemd.tmpfiles.rules = [
          "d /games 0755 runar users -"
        ];

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

        # Samsung USB-C headset negotiates full-speed only; its duplex profile
        # (mic + playback) exceeds full-speed USB bandwidth for the mic's
        # altsetting, causing a suspend/resume crash loop that took down
        # quickshell. Force output-only so the capture interface never opens.
        services.pipewire.wireplumber.extraConfig."51-usbc-headset-output-only" = {
          "device.profile.priority.rules" = [
            {
              matches = [
                { "device.name" = "alsa_card.usb-Samsung_USBC_Headset_20190816-00"; }
              ];
              actions.update-props.priorities = [ "output:analog-stereo" ];
            }
          ];
        };

        home-manager.users.runar = {
          home.packages = with pkgs; [
            bruno
          ];
        };
      };
  };
}
