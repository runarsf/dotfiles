{
  config,
  inputs,
  outputs,
  pkgs,
  system,
  hostname,
  ...
}:

let
  privateKeys = [
    "id_priv"
    "id_ntnu"
    "id_golog"
  ];
  keys = privateKeys ++ map (key: "${key}.pub") privateKeys;

  mkSecretFiles = map (key: {
    name = key;
    value = outputs.lib.mkSecretFile {
      source = "${inputs.vault}/runar/keys/${key}";
      destination = "${config.home.homeDirectory}/.ssh/${key}";
    };
  }) keys;

  secretFileDestinations = map (key: "${config.home.homeDirectory}/.ssh/${key}") privateKeys;

  secretFilesSet = builtins.listToAttrs mkSecretFiles;

in
# {
# sops = {
#   defaultSopsFile = "${inputs.vault}/secrets.yaml";
#   secrets = {
#     password_runar = {
#       sopsFile = "${inputs.vault}/passwords/runar";
#       neededForUsers = true;
#     };
#   };
# };
# } //
outputs.lib.mkFor system hostname {
  # hosts.rpi.sops.secrets = {
  # cloudflare = { sopsFile = "${inputs.vault}/secrets/cloudflare.txt"; };
  # };
  hosts.runix = {
    programs.ssh.matchBlocks."" = {
      identityFile = secretFileDestinations;
    };
    sops = {
      secrets = outputs.lib.deepMerge [
        secretFilesSet
        {
          monolisa = {
            sopsFile = "${inputs.vault}/shared/fonts/MonoLisa.zip";
            format = "binary";
          };
          dankmono = {
            sopsFile = "${inputs.vault}/shared/fonts/DankMono.zip";
            format = "binary";
          };
          operatormono = {
            sopsFile = "${inputs.vault}/shared/fonts/OperatorMono.zip";
            format = "binary";
          };
        }
      ];
    };
    programs.kitty.font.name = "Operator Mono Lig";
    stylix.targets.kitty.enable = false;
    systemd.user.services.myfonts = {
      Unit = {
        Description = "Fonts with stupid licenses";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = "${pkgs.writeShellScript "install-fonts" ''
          #!/run/current-system/sw/bin/bash
          set -o errexit
          set -o nounset

          ${pkgs.coreutils}/bin/mkdir -p "${config.xdg.dataHome}/fonts"
          ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.monolisa.path}" -d "${config.xdg.dataHome}/fonts"
          ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.dankmono.path}" -d "${config.xdg.dataHome}/fonts"
          ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.operatormono.path}" -d "${config.xdg.dataHome}/fonts"
          ${pkgs.fontconfig}/bin/fc-cache -f
        ''}";
      };
    };
  };
}
