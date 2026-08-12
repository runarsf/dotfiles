{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.anuc = lib'.mkHost {
    inherit self withSystem;
    configuration = {
      pkgs,
      lib,
      ...
    }: {
      imports = with self.nixosModules; [
        ./hardware-configuration.nix
        host
        homeManager
        runar
      ];

      system.stateVersion = "25.05";
      networking.hostName = "anuc";
    };
  };
}
