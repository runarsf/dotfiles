{ inputs, ... }:
{
  hosts.runix = {
    stateVersion = "24.05";
    users = [ "runar" ];
    modules = [
      "osu"
      "steam"
      "controllers"
    ];

    extraModules = [
      ./_hardware.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ];
  };
}
