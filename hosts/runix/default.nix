{
  self,
  inputs,
  withSystem,
  ...
}: {
  flake.nixosConfigurations.runix = withSystem "x86_64-linux" (
    {pkgs, ...}:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = [
          self.nixosModules.runixConfiguration
          self.nixosModules.homeManager
          {features.sops.vaultPath = inputs.vault;}
          {
            home-manager.users.runar = {config, ...}: {
              features.niks.flake = "${config.home.homeDirectory}/Development/dotfiles";
            };
          }
        ];
      }
  );

  flake.nixosModules.runixConfiguration = {
    pkgs,
    lib,
    ...
  }: {
    imports = with self.nixosModules; [
      runixHardware
      nix
      locales
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
