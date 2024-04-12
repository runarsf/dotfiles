flake@{ inputs, outputs, ... }:

let nixpkgsConfig = (import ../modules/nix/nixpkgs.nix flake).nixpkgs;

in rec {
  pkgsForSystem = system:
    import inputs.nixpkgs {
      inherit system;
      inherit (nixpkgsConfig) config overlays;
    };

  forEachSystem = systems: fn:
    outputs.lib.genAttrs systems (system: fn (pkgsForSystem system));

  addPatches = pkg: patches:
    pkg.overrideAttrs
    (prevAttrs: { patches = (prevAttrs.patches or [ ]) ++ patches; });
}
