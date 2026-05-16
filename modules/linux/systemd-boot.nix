{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.systemd-boot = _: {
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
