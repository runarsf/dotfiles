{ inputs, outputs, ... }:

{
  defaultSystem = "x86_64-linux";
  defaultStateVersion = "24.05";

  defaultNixpkgsConfig = {
    allowUnfree = true;
    allowUnfreePredicate = _: true;
    permittedInsecurePackages = [
      "electron-25.9.0"
    ];
  };

  defaultNixpkgsOverlays = [
    (_: prev: {
      master = import inputs.nixpkgs-master {
        inherit (prev) system;
        config = outputs.lib.defaultNixpkgsConfig;
      };
      unstable = import inputs.nixpkgs-unstable {
        inherit (prev) system;
        config = outputs.lib.defaultNixpkgsConfig;
      };
      nur = import inputs.nur {
        pkgs = prev;
        nurpkgs = import inputs.nixpkgs {
          inherit (prev) system;
          config = outputs.lib.defaultNixpkgsConfig;
        };
      };
      inputs = builtins.mapAttrs (_: flake:
        let
          legacyPackages = (flake.legacyPackages or { }).${prev.system} or { };
          packages = (flake.packages or { }).${prev.system} or { };
        in if legacyPackages != { } then legacyPackages else packages) inputs;
    })
  ];
}
