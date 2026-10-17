{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.anuc = lib'.mkHost {
    inherit self withSystem;

    hostName = "anuc";
    stateVersion = "25.05";
    users = ["runar"];

    imports = [./hardware-configuration.nix];
  };
}
