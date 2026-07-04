_: {
  flake.homeModules.c = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (builtins) readFile;
    inherit (lib) mkEnableOption optionals;
    inherit (pkgs.writers) writePython3Bin;

    cfg = config.features.c;
  in {
    options.features.c = {
      ide = mkEnableOption "C IDE";
    };

    config = {
      home.packages = with pkgs;
        [
          (with dotnetCorePackages; combinePackages [sdk_6_0 sdk_7_0 sdk_8_0])
          cmake
          gcc

          (writePython3Bin "find-cmake-target" {doCheck = false;} (readFile ./bin/find-cmake-target.py))
        ]
        ++ optionals cfg.ide [jetbrains.clion];
    };
  };
}
