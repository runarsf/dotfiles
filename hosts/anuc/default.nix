{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.anuc = let
    hostFeatures = lib'.useFeatures self [];
  in
    lib'.mkHost {
      inherit self withSystem;
      configuration = {
        pkgs,
        lib,
        ...
      }: {
        imports = with self.nixosModules;
          [
            ./hardware-configuration.nix
            host
            homeManager
            runar
          ]
          ++ hostFeatures.nixos;

        home-manager.users.runar.imports = hostFeatures.home;

        system.stateVersion = "25.05";
        networking.hostName = "anuc";
      };
    };
}
