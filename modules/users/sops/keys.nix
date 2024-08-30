{
  config,
  outputs,
  inputs,
  name,
  ...
}:

let
  toKeyPath = key: "${config.home.homeDirectory}/.ssh/${key}";

in
{
  options = {
    privateKeys = outputs.lib.mkOption {
      default = [];
      type = outputs.lib.types.listOf outputs.lib.types.str;
      description = "List of private key names";
    };
  };

  config =
    let
      privateKeyDestinations = map (key: toKeyPath key) config.privateKeys;
      secretFiles = builtins.listToAttrs (
        map (key: {
          name = key;
          value = outputs.lib.mkSecretFile {
            source = "${inputs.vault}/${name}/keys/${key}";
            destination = toKeyPath key;
          };
        }) config.privateKeys
      );

    in
    outputs.lib.mkIf (config.privateKeys != []) {
      programs.ssh.matchBlocks."".identityFile = privateKeyDestinations;
      sops.secrets = secretFiles;
    };
}
