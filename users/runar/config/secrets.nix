{ config, inputs, outputs, pkgs, system, hostname, ... }:

let
  keys = outputs.lib.concatMap (key: [ key "${key}.pub" ]) [
    "id_priv"
    "id_ntnu"
    "id_golog"
  ];

  mkSecretFiles = map (key: {
    name = key;
    value = outputs.lib.mkSecretFile {
      source = "${inputs.vault}/keys/${key}";
      destination = "${config.home.homeDirectory}/.ssh/${key}";
    };
  }) keys;

  secretFilesSet = builtins.listToAttrs mkSecretFiles;

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
in outputs.lib.mkFor system hostname {
  hosts.rpi.sops.secrets = {
    cloudflare = { sopsFile = "${inputs.vault}/secrets/cloudflare.txt"; };
  };
  hosts.runix = {
    sops = {
      secrets = outputs.lib.deepMerge [
        secretFilesSet
        {
          monolisa = {
            sopsFile = "${inputs.vault}/fonts/MonoLisa.zip";
            format = "binary";
          };
          dankmono = {
            sopsFile = "${inputs.vault}/fonts/DankMono.zip";
            format = "binary";
          };
          operatormono = {
            sopsFile = "${inputs.vault}/fonts/OperatorMono.zip";
            format = "binary";
          };
        }
      ];
    };
    programs.kitty.font.name = "Operator Mono Lig";
    stylix.targets.kitty.enable = false;
    systemd.user.services.myfonts = {
      Unit = { Description = "Fonts with stupid licenses"; };
      Install = { WantedBy = [ "default.target" ]; };
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
