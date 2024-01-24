{ config, inputs, outputs, system, hostname, ... }:

outputs.lib.mkFor system hostname {
  hosts.runix = {
    sops.secrets = {
      private_key = outputs.lib.mkSecretFile {
        source = "${inputs.vault}/ssh/id_priv";
        destination = "${config.home.homeDirectory}/.ssh/id_priv";
      };
      public_key = outputs.lib.mkSecretFile {
        source = "${inputs.vault}/ssh/id_priv.pub";
        destination = "${config.home.homeDirectory}/.ssh/id_priv.pub";
      };
    };
  };
}
