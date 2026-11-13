{
  self,
  inputs,
  lib,
  lib',
  config,
  withSystem,
  ...
}:
let
  inherit (lib) mkOption types mapAttrs genAttrs;

  hostModule = {
    options = {
      system = mkOption {
        type = types.str;
        default = "x86_64-linux";
      };
      stateVersion = mkOption { type = types.str; };
      users = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Names from `users` that get an account on this host.";
      };
      features = mkOption {
        type = lib'.featuresType;
        default = [ ];
      };
      modules = mkOption {
        type = types.listOf types.deferredModule;
        default = [ ];
        description = "Modules that aren't features, like hardware-configuration.nix.";
      };
      configuration = mkOption {
        type = types.deferredModule;
        default = { };
        description = "The host's own settings.";
      };
    };
  };

  mkHost =
    hostName: host:
    let
      hostFeatures = lib'.useFeatures self host.features;
    in
    withSystem host.system (
      { pkgs, ... }:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = [
          self.nixosModules.host
          self.nixosModules.homeManager
          {
            networking.hostName = hostName;
            system.stateVersion = host.stateVersion;
            home-manager.users = genAttrs host.users (_: {
              imports = hostFeatures.home;
            });
          }
          host.configuration
        ]
        ++ map (user: self.nixosModules.${user}) host.users
        ++ hostFeatures.nixos
        ++ host.modules;
      }
    );
in
{
  options.hosts = mkOption {
    type = types.attrsOf (types.submodule hostModule);
    default = { };
    description = "NixOS hosts, each built into nixosConfigurations.<name>.";
  };

  config.flake.nixosConfigurations = mapAttrs mkHost config.hosts;
}
