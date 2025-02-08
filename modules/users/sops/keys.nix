{ config, outputs, inputs, name, ... }:

let
  toKeyPath = key: "${config.home.homeDirectory}/.ssh/${key}";
  privateKeyDestinations =
    map (key: toKeyPath key) config.modules.sops.privateKeys;
  secretFiles = builtins.listToAttrs (map (key: {
    name = key;
    value = outputs.lib.mkSecretFile {
      source = "${inputs.vault}/${name}/keys/${key}";
      destination = toKeyPath key;
    };
  }) config.modules.sops.privateKeys);

in outputs.lib.mkIf (config.modules.sops.privateKeys != [ ]) {
  programs.ssh.matchBlocks."".identityFile = privateKeyDestinations;
  sops.secrets = secretFiles;
}
