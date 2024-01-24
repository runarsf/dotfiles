{ inputs, outputs, ... }:

rec {
  nix = {
    nixPath = [ "/etc/nix/path" ];
    registry = outputs.lib.mapAttrs (_: value: { flake = value; }) inputs;
  };

  environment.etc = outputs.lib.mapAttrs' (name: value: {
    name = "nix/path/${name}";
    value.source = value.flake;
  }) nix.registry;
}
