{lib', ...}: {
  flake.homeModules.modding = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkOption types;
    inherit (lib'.matching) matchStringList;

    cfg = config.features.modding;
  in {
    options.features.modding = with types; {
      games = mkOption {
        type = listOf <| enum ["unity" "celeste" "outer wilds"];
        default = [];
      };
    };

    config = {
      home.packages = with pkgs;
        matchStringList cfg.games [
          ["unity" r2modman]
          ["celeste" olympus]
          ["outer wilds" owmods-gui]
        ];
    };
  };
}
