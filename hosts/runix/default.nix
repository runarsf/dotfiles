{
  self,
  inputs,
  withSystem,
  lib',
  ...
}: let
  hostFeatures = lib'.useFeatures self ["osu" "steam" "controllers"];
in {
  flake.nixosConfigurations.runix = lib'.mkHost {
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

      networking.hostName = "runix";
      system.stateVersion = "24.05";

      home-manager.users.runar.imports = hostFeatures.home;
    };
  };
}
