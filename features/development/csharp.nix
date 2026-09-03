_: {
  flake.homeModules.csharp = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkEnableOption optionals;

    cfg = config.features.csharp;
  in {
    options.features.csharp = {
      ide = mkEnableOption "C# IDE";
    };

    config = {
      home.packages = with pkgs;
        [
          (with dotnetCorePackages; combinePackages [sdk_10_0])
        ]
        ++ optionals cfg.ide [jetbrains.rider];
    };
  };
}
