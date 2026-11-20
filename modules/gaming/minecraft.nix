{ lib', ... }: {
  flake.homeModules.minecraft =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib) mkOption toList types;
      inherit (lib'.matching) matchStringList;

      cfg = config.modules.minecraft;
    in
    {
      options.modules.minecraft = with types; {
        launcher = mkOption {
          type =
            let
              launchers = enum [
                "prism"
                "modrinth"
              ];
            in
            either launchers (listOf launchers);
          default = "prism";
          apply = toList;
        };
      };

      config = {
        home.packages =
          with pkgs;
          matchStringList cfg.launcher [
            [
              "prism"
              prismlauncher
            ]
            [
              "modrinth"
              modrinth-app
            ]
          ];
      };
    };
}
