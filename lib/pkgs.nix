{ inputs, outputs, ... }:

rec {
  pkgsForSystem = system:
    import inputs.nixpkgs {
      inherit system;
      config = outputs.lib.defaultNixpkgsConfig;
      overlays = outputs.lib.defaultNixpkgsOverlays;
    };

  forEachSystem = systems: fn:
    outputs.lib.genAttrs systems (system: fn (pkgsForSystem system));

  addPatches = pkg: patches:
    pkg.overrideAttrs
    (prevAttrs: { patches = (prevAttrs.patches or [ ]) ++ patches; });
}
