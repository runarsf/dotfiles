{ outputs, ... }:

{
  nixpkgs = {
    config = outputs.lib.defaultNixpkgsConfig;
    overlays = outputs.lib.defaultNixpkgsOverlays;
  };
}
