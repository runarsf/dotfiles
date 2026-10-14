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
    "sable"
  ];
  defaultClients = [
    "element"
    "sable"
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
          default = defaultClients;
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
          default = defaultClients;
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
            [
              "sable"
              self.packages.${system}.sable
            ]
          ];

        programs.element-desktop.enable = elem "element" cfg.clients;
      };
    };
}
