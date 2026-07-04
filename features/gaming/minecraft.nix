{lib', ...}: {
  flake.homeModules.minecraft = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) mkOption toList types;
    inherit (lib'.matching) matchStringList;

    cfg = config.features.minecraft;
  in {
    options.features.minecraft = with types; {
      launcher = mkOption {
        type = let
          launchers = enum ["prism" "modrinth"];
        in
          either launchers (listOf launchers);
        default = "prism";
        apply = toList;
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
