{
  self,
  inputs,
  withSystem,
  lib',
  ...
}: let
  hostFeatures = lib'.useFeatures self [
    "steam"
    "godot"
  ];
in {
  flake.nixosConfigurations.toaster = lib'.mkHost {
    inherit self withSystem;

    configuration = _: {
      imports = with self.nixosModules;
        [
          ./hardware-configuration.nix
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490s
          host
          homeManager
          thomas
        ]
        ++ hostFeatures.nixos;

      networking.hostName = "toaster";
      system.stateVersion = "24.05";

      features.niri.overrides = {
        outputs."eDP-1".scale = 1;
      };

      home-manager.users.thomas.imports = hostFeatures.home;
    };
  };
}
