{ inputs, ... }:
{
  hosts.runix = {
    stateVersion = "24.05";
    users = [ "runar" ];
    features = [
      "osu"
      "steam"
      "controllers"
    ];

    modules = [
      ./_hardware.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ];
  };
}
