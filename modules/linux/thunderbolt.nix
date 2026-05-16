{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.thunderbolt = _: {
    services.hardware.bolt.enable = true;
  };
}
