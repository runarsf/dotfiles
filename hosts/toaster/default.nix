{
  self,
  inputs,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.toaster = lib'.mkHost {
    inherit self withSystem;
    configuration = _: {
      imports = with self.nixosModules; [
        ./hardware-configuration.nix
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490s
        host
        homeManager
        thomas
      ];

      system.stateVersion = "24.05";
      networking.hostName = "toaster";

      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;
    };
  };
}
