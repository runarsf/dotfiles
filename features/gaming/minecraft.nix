_: {
  flake.homeModules.minecraft = {
    config,
    inputs,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) mkOption toList types;
    inherit (inputs.nixlib.lib.matching) matchStringList;

    cfg = config.features.minecraft;
  in {
    options.features.minecraft = with types; {
      launcher = mkOption {
        type = let
          launchers = enum ["prism" "modrinth"];
        in
          either launchers (listOf launchers);
        default = "prism";
        apply = x: toList x;
      };
    };

    config = {
      home.packages = with pkgs;
        matchStringList cfg.launcher [
          ["prism" prismlauncher]
          ["modrinth" modrinth-app]
        ];
    };
  };
}
