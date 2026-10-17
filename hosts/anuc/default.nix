{
  self,
  withSystem,
  lib',
  ...
}: let
  hostFeatures = lib'.useFeatures self [];
in {
  flake.nixosConfigurations.anuc = lib'.mkHost {
    inherit self withSystem;

    configuration = _: {
      imports = with self.nixosModules;
        [
          ./hardware-configuration.nix
          host
          homeManager
          runar
        ]
        ++ hostFeatures.nixos;

      networking.hostName = "anuc";
      system.stateVersion = "25.05";

      home-manager.users.runar.imports = hostFeatures.home;
    };
  };
}
