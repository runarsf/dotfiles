{
  self,
  inputs,
  withSystem,
  lib',
  ...
}:
{
  flake.nixosConfigurations.runix = lib'.mkHost {
    inherit self withSystem;

    hostName = "runix";
    stateVersion = "24.05";
    users = [ "runar" ];
    features = [
      "osu"
      "steam"
      "controllers"
    ];

    imports = [
      ./hardware-configuration.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ];
  };
}
