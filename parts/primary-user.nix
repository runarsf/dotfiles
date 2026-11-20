{ lib', ... }: {
  flake.nixosModules.primaryUser = lib'.mkModule "primaryUser" (
    { lib, ... }: {
      options.primaryUsers = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Usernames of the primary human users.";
      };
    }
  );
}
