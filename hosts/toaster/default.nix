{ inputs, ... }:
{
  hosts.toaster = {
    stateVersion = "24.05";
    users = [ "thomas" ];
    modules = [
      "steam"
      "godot"
    ];

    extraModules = [
      ./_hardware.nix
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t490s
    ];

    configuration = _: {
      modules.niri.overrides.outputs."eDP-1".scale = 1;
      services.throttled.enable = false;
    };
  };
}
