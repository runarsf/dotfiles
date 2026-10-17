{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.vm = lib'.mkHost {
    inherit self withSystem;

    hostName = "vm";
    stateVersion = "25.11";
    users = ["runar"];
    features = ["sshServer"];

    imports = [./hardware-configuration.nix];

    configuration = {pkgs, ...}: {
      host.efi = false;

      boot.loader.grub = {
        enable = true;
        device = "/dev/vda";
        useOSProber = true;
      };

      services.xserver.enable = true;
      services.displayManager.sddm.enable = true;
      services.desktopManager.plasma6.enable = true;

      # Home-only features otherwise, so root has no copy.
      environment.systemPackages = with pkgs; [
        git
        vim
      ];

      home-manager.users.runar = {config, ...}: {
        features.niks.flake = "${config.home.homeDirectory}/shared/dotfiles";
      };
    };
  };
}
