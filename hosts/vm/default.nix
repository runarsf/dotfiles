{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.vm = let
    hostFeatures = lib'.useFeatures self [];
  in
    lib'.mkHost {
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

        home-manager.users.runar.imports = hostFeatures.home;

        system.stateVersion = "25.11";
        networking.hostName = "vm";

        boot.loader.grub.enable = true;
        boot.loader.grub.device = "/dev/vda";
        boot.loader.grub.useOSProber = true;

        networking.networkmanager.enable = true;
        services.xserver.enable = true;
        services.displayManager.sddm.enable = true;
        services.desktopManager.plasma6.enable = true;

        environment.systemPackages = with pkgs; [
          git
          vim
        ];
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
