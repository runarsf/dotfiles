{
  inputs,
  lib,
  config,
  ...
}: let
  inherit (lib) mkOption;
  inherit (lib.types) listOf raw;
  inherit (inputs.nixlib.lib) deepMerge;
in {
  options = {
    libExtensions = mkOption {
      type = listOf raw;
      default = [];
    };
  };

  config = let
    lib' = deepMerge (
      [
        lib
        inputs.home-manager.lib
        inputs.nixlib.lib
      ]
      ++ config.libExtensions
    );
  in {
    # Support both passing as an argument and using as a dendrite.
    _module.args = {
      inherit lib';
    };
    flake = {
      inherit lib';
    };
  };
}
