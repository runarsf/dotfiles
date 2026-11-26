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
  inherit (builtins) attrNames;
  inherit (lib)
    mkOption
    types
    mapAttrs
    mapAttrs'
    mapAttrsToList
    nameValuePair
    genAttrs
    catAttrs
    concatMap
    concatStringsSep
    subtractLists
    ;

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
      modules = mkOption {
        type = lib'.modulesType;
        default = [ ];
      };
      extraModules = mkOption {
        type = types.listOf types.deferredModule;
        default = [ ];
        description = "Modules that aren't in modules/, like _hardware.nix.";
      };
      root = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "Directory whose _config/ holds extra { homeModule?, nixosModule? } files. A host has several users, so homeModule is an attrset of username -> module.";
      };
      configuration = mkOption {
        type = types.deferredModule;
        default = { };
        description = "The host's own settings.";
      };
    };
  };

  # The host's modules together with its _config/ files, with the home half
  # already split per user.
  resolve =
    hostName: host:
    let
      hostModules = lib'.useModules self host.modules;
      entries = lib'.readConfigDir host.root;
      homeModules = catAttrs "homeModule" entries;
      unknown = subtractLists host.users (concatMap attrNames homeModules);
    in
    if unknown != [ ] then
      throw "hosts.${hostName}: _config/ sets homeModule for ${concatStringsSep ", " unknown}, not in users (${concatStringsSep ", " host.users})"
    else
      {
        nixos = hostModules.nixos ++ catAttrs "nixosModule" entries;
        home = genAttrs host.users (username: hostModules.home ++ catAttrs username homeModules);
      };

  mkHost =
    hostName: host:
    let
      resolved = resolve hostName host;
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
            home-manager.users = mapAttrs (_: imports: { inherit imports; }) resolved.home;
          }
          host.configuration
        ]
        ++ map (user: self.nixosModules.${user}) host.users
        ++ resolved.nixos
        ++ host.extraModules;
      }
    );

  # homeConfigurations."<user>@<host>": the user's own home config with the
  # host's home modules on top, since homeConfigurations.<user> has no host.
  mkHostHomeConfigurations =
    hostName: host:
    mapAttrs' (
      username: hostHomeModules:
      nameValuePair "${username}@${hostName}" (
        withSystem host.system (
          { pkgs, ... }:
          inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;

            extraSpecialArgs = { inherit self; };
            modules = [
              self.homeModules.${username}
              {
                home = {
                  inherit username;
                  inherit (config.users.${username}) homeDirectory;
                };
              }
            ]
            ++ hostHomeModules;
          }
        )
      )
    ) (resolve hostName host).home;
in
{
  options.hosts = mkOption {
    type = types.attrsOf (types.submodule hostModule);
    default = { };
    description = "NixOS hosts, each built into nixosConfigurations.<name>.";
  };

  config.flake = {
    nixosConfigurations = mapAttrs mkHost config.hosts;
    homeConfigurations = lib.mkMerge (mapAttrsToList mkHostHomeConfigurations config.hosts);
  };
}
