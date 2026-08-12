{
  self,
  inputs,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.runix = let
    hostFeatures = lib'.useFeatures self ["osu" "steam" "controllers"];
  in
    lib'.mkHost {
      inherit self withSystem;
      configuration = _: {
        imports = with self.nixosModules;
          [
            ./hardware-configuration.nix
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
            host
            homeManager
            runar
          ]
          ++ hostFeatures.nixos;

        home-manager.users.runar.imports = hostFeatures.home;

        system.stateVersion = "24.05";
        networking.hostName = "runix";

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
      };
    };
}
