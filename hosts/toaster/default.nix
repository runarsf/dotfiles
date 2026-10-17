{
  self,
  inputs,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.toaster = lib'.mkHost {
    inherit self withSystem;

    hostName = "toaster";
    stateVersion = "24.05";
    users = ["thomas"];
    features = ["steam" "godot"];

    imports = [
      ./hardware-configuration.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490s
    ];

    configuration = _: {
      features.niri.overrides.outputs."eDP-1".scale = 1;
    };
  };
}
