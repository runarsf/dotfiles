{ config, inputs, outputs, system, hostname, ... }:

outputs.lib.mkFor system hostname {
  hosts.runix = {
    sops.secrets = {
      sshConfig = outputs.lib.mkSecretFile {
        source = "${inputs.vault}/ssh/config";
        destination = "${config.home.homeDirectory}/.ssh/config";
      };
    };
  };
}
