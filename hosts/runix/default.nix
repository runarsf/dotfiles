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
        modules = with self.nixosModules; [
          runixConfiguration
          homeManager
        ];
      }
  );

  flake.nixosModules.runixConfiguration = _: {
    imports = with self.nixosModules; [
      ./hardware-configuration.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
      homeManager
      runar
    ];

    system.stateVersion = "24.05";
    networking.hostName = "runix";

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
