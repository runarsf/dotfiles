{
  self,
  inputs,
  withSystem,
  ...
}: {
  flake.nixosConfigurations.toaster = withSystem "x86_64-linux" (
    {pkgs, ...}:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = with self.nixosModules; [
          toasterConfiguration
          homeManager
        ];
      }
  );

  flake.nixosModules.toasterConfiguration = _: {
    imports = with self.nixosModules; [
      ./hardware-configuration.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490s
      homeManager
      thomas
    ];

    system.stateVersion = "24.05";
    networking.hostName = "toaster";

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
