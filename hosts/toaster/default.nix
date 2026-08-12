{
  self,
  inputs,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.toaster = let
    hostFeatures = lib'.useFeatures self [];
  in
    lib'.mkHost {
      inherit self withSystem;
      configuration = _: {
        imports = with self.nixosModules;
          [
            ./hardware-configuration.nix
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490s
            host
            homeManager
            thomas
          ]
          ++ hostFeatures.nixos;

        home-manager.users.thomas.imports = hostFeatures.home;

        system.stateVersion = "24.05";
        networking.hostName = "toaster";

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
      };
    };
}
