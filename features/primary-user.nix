_: {
  flake.nixosModules.primaryUser = {lib, ...}: let
    inherit (lib) types;
  in {
    options.primaryUsers = with types;
      lib.mkOption {
        type = listOf str;
        default = throw "Set 'primaryUsers' to a list of usernames in your user module";
        description = "Usernames of the primary human users. Used by features to grant group memberships.";
      };
  };
}
