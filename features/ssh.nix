_: {
  flake.nixosModules.ssh = {lib, ...}: {
    options.features.ssh.keys = lib.mkOption {
      default = [];
      description = "Public SSH keys. Added as authorized keys for all primaryUsers. First key is the default signing key.";
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Key name, used as the filename (~/.ssh/<name>.pub).";
          };
          key = lib.mkOption {
            type = lib.types.str;
            description = "Public key string.";
          };
        };
      });
    };

    /*
    config = lib.mkIf (cfg.keys != []) {
        users.users = lib.genAttrs config.primaryUsers (_: {
          openssh.authorizedKeys.keys = map (k: k.key) cfg.keys;
        });
      };
    */
  };

  flake.homeModules.ssh = {
    lib,
    osConfig ? {},
    ...
  }: let
    keys = osConfig.features.ssh.keys or [];
  in {
    config = {
      programs.ssh = {
        enable = true;
        matchBlocks."*".addKeysToAgent = "yes";
      };

      home.file = builtins.listToAttrs (map (k:
        lib.nameValuePair ".ssh/${k.name}.pub" {text = k.key;})
      keys);
    };
  };
}
