{
  self,
  inputs,
  lib',
  ...
}: {
  flake.homeModules.go = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkEnableOption optionals;
    inherit (pkgs.stdenv.hostPlatform) system;

    cfg = config.features.android;
  in {
    options.features.go = {
      ide = mkEnableOption "Go IDE";
    };

    config = {
      home.packages = with pkgs;
        [
          go
          (self.packages.${system}.gorun)
        ]
        ++ optionals cfg.ide [jetbrains.goland];
    };
  };
}
