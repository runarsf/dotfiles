{
  self,
  inputs,
  ...
}: {
  flake.nixosConfigurations.runix = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.nixosModules.runixConfiguration
    ];
  };
}
