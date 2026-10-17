{
  self,
  withSystem,
  lib',
  ...
}: let
  hostFeatures = lib'.useFeatures self [];
in {
  flake.nixosConfigurations.vm = lib'.mkHost {
    inherit self withSystem;

    configuration = {pkgs, ...}: {
      imports = with self.nixosModules;
        [
          ./hardware-configuration.nix
          host
          homeManager
          runar
          sshServer
        ]
        ++ hostFeatures.nixos;

      networking.hostName = "vm";
      system.stateVersion = "25.11";

      boot.loader.grub = {
        enable = true;
        device = "/dev/vda";
        useOSProber = true;
      };

      services.xserver.enable = true;
      services.displayManager.sddm.enable = true;
      services.desktopManager.plasma6.enable = true;

      # git and vim are home-only features here, so root has no copy otherwise.
      environment.systemPackages = with pkgs; [
        git
        vim
      ];

      home-manager.users.runar.imports = hostFeatures.home;
    };

    extraModules = [
      {
        home-manager.users.runar = {config, ...}: {
          features.niks.flake = "${config.home.homeDirectory}/shared/dotfiles";
        };
      }
    ];
  };
}
