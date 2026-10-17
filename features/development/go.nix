{ self, ... }: {
  flake.homeModules.go =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib) mkEnableOption optionals;
      inherit (pkgs.stdenv.hostPlatform) system;

      cfg = config.features.go;
    in
    {
      options.features.go = {
        ide = mkEnableOption "Go IDE";
      };

      config = {
        home.packages =
          with pkgs;
          [
            go
            gcc
            (self.packages.${system}.gorun)
          ]
          ++ optionals cfg.ide [ jetbrains.goland ];
      };
    };
}
