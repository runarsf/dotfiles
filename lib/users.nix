flake@{ outputs, ... }:

{
  # TODO Add osConfig: https://github.com/imatpot/dotfiles/commit/bc72621c353166667a4d8ecb3f5454d9f5c6de5f
  #  Note that stylix doesn't work with this and causes an infinite recursion.

  mkUser = args@{ username, hostname ? null, system ? outputs.lib.defaultSystem
    , stateVersion ? outputs.lib.defaultStateVersion, ... }:
    outputs.lib.homeManagerConfiguration {
      pkgs = outputs.lib.pkgsForSystem system;

      extraSpecialArgs = flake // args // {
        # https://nixos.wiki/wiki/Nix_Language_Quirks#Default_values_are_not_bound_in_.40_syntax
        inherit system hostname stateVersion;

        osConfig = null;

        # Actual name required by submodules. This makes sure everything is
        # interopable across NixOS & non-NixOS systems.
        # https://github.com/nix-community/home-manager/blob/ca4126e3c568be23a0981c4d69aed078486c5fce/nixos/common.nix#L22
        name = username;
      };

      modules = [
        ../modules/home-manager/shared.nix
        ../modules/home-manager/default-config.nix
        ../modules/nix/nix.nix
        ../users/${username}/home.nix
      ];
    };
}
