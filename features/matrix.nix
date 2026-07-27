{
  lib',
  self,
  ...
}:
let
  clients = lib'.types.enum [
    "element"
    "cinny"
    "commet"
  ];
in
{
  flake.nixosModules.matrix =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib) mkOption toList types;
      inherit (lib'.matching) matchStringList;
      inherit (pkgs.stdenv.hostPlatform) system;

      cfg = config.features.matrix;
    in
    {
      options.features.matrix = with types; {
        clients = mkOption {
          type = either clients (listOf clients);
          default = [ ];
          apply = toList;
        };
      };

      config = {
        services.flatpak = {
          packages = matchStringList cfg.clients [
            [
              "commet"
              self.packages.${system}.commet
            ]
          ];
        };
      };
    };

  flake.homeModules.matrix =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (builtins) elem;
      inherit (lib) mkOption toList types;
      inherit (lib'.matching) matchStringList;

      cfg = config.features.matrix;
    in
    {
      options.features.matrix = with types; {
        clients = mkOption {
          type = either clients (listOf clients);
          default = "element";
          apply = toList;
        };
      };

      config = {
        home.packages =
          with pkgs;
          matchStringList cfg.clients [
            [
              "cinny"
              cinny-desktop
            ]
          ];

        programs.element-desktop.enable = elem "element" cfg.clients;
      };
    };
}
