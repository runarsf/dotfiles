{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.bramble = let
    hostFeatures = lib'.useFeatures self [
      "yubikey"
      {
        csharp = {
          ide = true;
        };
      }
      # {
      #   hyprland = {
      #     animations = true;
      #     nvidia = true;
      #   };
      # }
    ];
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
        home-manager.users.runar.features.hyprland.animations = true;

        system.stateVersion = "26.05";
        networking.hostName = "bramble";

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
          open = true;
          nvidiaSettings = true;
          package = config.boot.kernelPackages.nvidiaPackages.production;

          prime = {
            offload.enable = true;
            offload.enableOffloadCmd = true;
            amdgpuBusId = "PCI:6:0:0";
            nvidiaBusId = "PCI:1:0:0";
          };
        };
        services.xserver.xkb.layout = "no";
        services.xserver.xkb.variant = "nb";
        services.libinput.enable = true;
        console = {
          font = "Lat2-Terminus16";
          useXkbConfig = true; # use xkb.options in tty.
        };
        services.pipewire = {
          enable = true;
          pulse.enable = true;
        };
        networking.networkmanager.enable = true;

        environment.systemPackages = with pkgs; [
          teams-for-linux
        ];
      };
    };
}
