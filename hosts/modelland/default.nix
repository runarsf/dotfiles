{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.modelland = let
    hostFeatures = lib'.useFeatures self ["osu" "steam" "controllers"];
  in
    lib'.mkHost {
      inherit self withSystem;
      configuration = {
        config,
        pkgs,
        ...
      }: {
        imports = with self.nixosModules;
          [
            ./hardware-configuration.nix
            host
            homeManager
            runar
          ]
          ++ hostFeatures.nixos;

        home-manager.users.runar.imports = hostFeatures.home;
        home-manager.users.runar.features.hyprland.nvidia = true;

        system.stateVersion = "26.05";
        networking.hostName = "modelland";

        features.niri.overrides.outputs."GIGA-BYTE TECHNOLOGY CO., LTD. GO27Q24G 26112F001094" = {
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

        # 1. Never idle-suspend the card's nodes → no re-open blips
        services.pipewire.wireplumber.extraConfig."51-no-suspend" = {
          "monitor.alsa.rules" = [
            {
              matches = [
                {"node.name" = "~alsa_output.*";}
                {"node.name" = "~alsa_input.*";}
              ];
              actions.update-props."session.suspend-timeout-seconds" = 0;
            }
          ];
        };

        # 2. Pin the card to the duplex profile so it can't drop to Off
        # Find card name and profile name from `pactl list cards`
        systemd.user.services.pin-audio-profile = {
          wantedBy = ["wireplumber.service"];
          after = ["wireplumber.service"];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStartPre = "${pkgs.coreutils}/bin/sleep 2";
            ExecStart = "${pkgs.pulseaudio}/bin/pactl set-card-profile 'alsa_card.usb-Samsung_USBC_Headset_20190816-00' 'output:analog-stereo+input:mono-fallback'";
          };
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
      };
    };
}
