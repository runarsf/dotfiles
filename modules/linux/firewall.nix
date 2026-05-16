{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.firewall = _: {
    networking.firewall = {
      enable = true;
    };
  };
}
