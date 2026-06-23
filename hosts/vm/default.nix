{
  self,
  inputs,
  withSystem,
  ...
}: {
  flake.nixosConfigurations.vm = withSystem "x86_64-linux" (
    {pkgs, ...}:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = with self.nixosModules; [
          vmConfiguration
          homeManager
          {
            home-manager.users.runar = {config, ...}: {
              features.niks.flake = "${config.home.homeDirectory}/shared/dotfiles";
            };
          }
        ];
      }
  );

  flake.nixosModules.vmConfiguration = {pkgs, ...}: {
    imports = with self.nixosModules; [
      ./hardware-configuration.nix
      homeManager
      runar
      sshServer
    ];

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
}
