{
  self,
  inputs,
  withSystem,
  ...
}: {
  flake.nixosConfigurations.anuc = withSystem "x86_64-linux" (
    {pkgs, ...}:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = with self.nixosModules; [
          anucConfiguration
          homeManager
        ];
      }
  );

  flake.nixosModules.anucConfiguration = {
    pkgs,
    lib,
    ...
  }: {
    imports = with self.nixosModules; [
      ./hardware-configuration.nix
      homeManager
      runar
    ];

    system.stateVersion = "25.05";
    networking.hostName = "anuc";
  };
}
