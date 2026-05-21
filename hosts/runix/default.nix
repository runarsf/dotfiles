{
  self,
  inputs,
  ...
}: {
  flake.nixosConfigurations.runix = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.nixosModules.runixConfiguration
      self.nixosModules.homeManager
      {features.sops.vaultPath = inputs.vault;}
    ];
  };

  flake.nixosModules.runixConfiguration = {
    pkgs,
    lib,
    ...
  }: {
    imports = with self.nixosModules; [
      runixHardware
      nix
      niri
      homeManager
      runar

      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ];

    system.stateVersion = "24.05";

    environment.systemPackages = with pkgs; [
      firefox
      vim
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
