args@{ outputs, users, ... }:

let
  importUserConfig = name: {
    imports = [
      ../../users/${name}/home.nix

      # This works because the Home Manager NixOS module passes `name`, which is
      # the attribute name of `users.<name>`, to all submodules automatically.
      # https://github.com/nix-community/home-manager/blob/ca4126e3c568be23a0981c4d69aed078486c5fce/nixos/common.nix#L22
      ../home-manager/default-config.nix
    ];
  };

in {
  home-manager = {
    users = outputs.lib.genAttrs users importUserConfig;

    extraSpecialArgs = {
      # The `config` passed from NixOS would override Home Manager's `config`,
      # so we re-expose every attribute except `config`.
      # https://github.com/nix-community/home-manager/blob/ca4126e3c568be23a0981c4d69aed078486c5fce/nixos/common.nix#L20
      inherit (args) inputs outputs hostname system stateVersion;
    };

    sharedModules = [ ../home-manager/shared.nix ];

    # Prevents NixOS & non-NixOS user configurations from diverging.
    # https://discourse.nixos.org/t/home-manager-useuserpackages-useglobalpkgs-settings/34506/4
    useGlobalPkgs = false;
    useUserPackages = false;
  };
}
