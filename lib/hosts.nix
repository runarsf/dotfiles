flake@{ inputs, outputs, ... }:

{
  mkHost = args@{ hostname, system ? outputs.lib.defaultSystem
    , stateVersion ? outputs.lib.defaultStateVersion, users ? [ ], ... }:
    outputs.lib.nixosSystem {
      inherit system;

      specialArgs = flake // args // {
        # https://nixos.wiki/wiki/Nix_Language_Quirks#Default_values_are_not_bound_in_.40_syntax
        inherit system stateVersion users;
      };

      modules = [
        inputs.home-manager.nixosModules.home-manager
        inputs.sops-nix.nixosModules.sops

        ../hosts/${hostname}/configuration.nix
        ../hosts/${hostname}/hardware-configuration.nix

        ../modules/nix/nix.nix
        ../modules/nix/nixpkgs.nix

        ../modules/nixos/nix-legacy-consistency.nix
        ../modules/nixos/default-config.nix

        ../modules/nixos/home-manager.nix
        ../modules/nixos/user-system-configs.nix
      ];
    };
}
