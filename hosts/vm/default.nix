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
        modules = [
          self.nixosModules.vmConfiguration
          self.nixosModules.homeManager
          {features.sops.vaultPath = inputs.vault;}
        ];
      }
  );

  flake.nixosModules.vmConfiguration = {
    pkgs,
    lib,
    ...
  }: {
    imports = with self.nixosModules; [
      vmHardware
      nix
      locales
      homeManager
      runar
    ];

    system.stateVersion = "25.11";

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/vda";
    boot.loader.grub.useOSProber = true;

    networking.hostName = "vm";
    networking.networkmanager.enable = true;
    services.xserver.enable = true;
    services.displayManager.sddm.enable = true;
    services.desktopManager.plasma6.enable = true;
    services.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    programs.firefox.enable = true;

    environment.systemPackages = with pkgs; [
      git
      vim
    ];
  };
}
