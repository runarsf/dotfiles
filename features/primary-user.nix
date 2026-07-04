_: {
  flake.nixosModules.primaryUser = {lib, config, ...}: let
    inherit (lib) types;
  in {
    options.primaryUsers = with types;
      lib.mkOption {
        type = listOf str;
        default = lib.attrNames (lib.filterAttrs (_: u: u.isNormalUser) config.users.users);
        description = "Usernames of the primary human users. Used by features to grant group memberships.";
      };
  };
}
