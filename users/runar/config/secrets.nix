{ config, inputs, outputs, system, hostname, ... }:

{
  # sops = {
  #   defaultSopsFile = "${inputs.vault}/secrets.yaml";
  #   secrets = {
  #     password_runar = {
  #       sopsFile = "${inputs.vault}/passwords/runar";
  #       neededForUsers = true;
  #     };
  #   };
  # };
  # } // outputs.lib.mkFor system hostname {
  # hosts.rpi = {
  #   sops.secrets = {
  #     cloudflare = {
  #       sopsFile = "${inputs.vault}/secrets/cloudflare.txt";
  #     };
  #   };
  # };
  # hosts.runix = {
  #   sops.secrets = {
  #     public_key = outputs.lib.mkSecretFile {
  #       source = "${inputs.vault}/ssh/id_priv.pub";
  #       destination = "${config.home.homeDirectory}/.ssh/id_priv.pub";
  #     };
  #   };
  # };
} 
