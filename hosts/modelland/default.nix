{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.modelland = lib'.mkHost {
    inherit self withSystem;
    configuration = {
      config,
      pkgs,
      ...
    }: let
      hostFeatures = lib'.useFeatures self [
        "osu"
        "steam"
        "controllers"
        "easyeffects"
        "go"
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
        features = {
          hyprland.nvidia = true;
        };
      };

      system.stateVersion = "26.05";
      networking.hostName = "modelland";

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
              {"device.name" = "alsa_card.usb-Samsung_USBC_Headset_20190816-00";}
            ];
            actions.update-props.priorities = ["output:analog-stereo"];
          }
        ];
      };

      services.power-profiles-daemon.enable = true;

      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
      boot.kernelPackages = pkgs.linuxPackages_latest;
      time.timeZone = "Europe/Zurich";
      zramSwap.enable = true;
      swapDevices = [{device = "/swap/swapfile";}];
      services.xserver.videoDrivers = ["nvidia"];
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
      };
      hardware.nvidia = {
        modesetting.enable = true;
        open = false;
        nvidiaSettings = true;
        powerManagement.enable = false;
        package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
      };
      # services.desktopManager.plasma6.enable = true;
      # services.displayManager.plasma-login-manager.enable = true;
      # services.desktopManager.gnome.enable = true;
      # services.displayManager.gdm.enable = true;
      services.xserver.xkb.layout = "no";
      services.xserver.xkb.variant = "nb";
      console = {
        font = "Lat2-Terminus16";
        useXkbConfig = true; # use xkb.options in tty.
      };
      services.pipewire = {
        enable = true;
        pulse.enable = true;
      };
      networking.networkmanager.enable = true;

      systemd.tmpfiles.rules = [
        "d /games 0755 runar users -"
      ];

      environment.systemPackages = with pkgs; [
        bruno
        caffeine-ng
      ];
    };
  };
}
