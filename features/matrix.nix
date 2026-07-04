{
  lib',
  self,
  ...
}: {
  flake.nixosModules.matrix = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkOption toList types;
    inherit (lib'.matching) matchStringList;
    inherit (pkgs.stdenv.hostPlatform) system;

    cfg = config.features.matrix;
  in {
    options.features.matrix = with types; {
      client = mkOption {
        type = let
          clients = enum ["commet"];
        in
          either clients (listOf clients);
        default = [];
        apply = toList;
      };
    };

    config = {
      services.flatpak = {
        packages = matchStringList cfg.client [
          ["commet" self.packages.${system}.commet]
        ];
      };
    };
  };

  flake.homeModules.matrix = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (builtins) elem;
    inherit (lib) mkOption toList types;
    inherit (lib'.matching) matchStringList;

    cfg = config.features.matrix;
  in {
    options.features.matrix = with types; {
      client = mkOption {
        type = let
          clients = enum ["element" "cinny"];
        in
          either clients (listOf clients);
        default = "element";
        apply = toList;
      };
    };

    config = {
      home.packages = with pkgs;
        matchStringList cfg.client [
          ["cinny" cinny-desktop]
        ];

      programs.element-desktop.enable = elem "element" cfg.clients;
    };
  };
}
