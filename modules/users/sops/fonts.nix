{
  config,
  inputs,
  outputs,
  pkgs,
  system,
  hostname,
  ...
}:

# This is a module that utilizes the shared sops secrets in the vault.

outputs.lib.mkDesktopModule config "sops-fonts" {
  sops.secrets = {
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
  };

  programs.kitty.font.name = "Operator Mono Lig";
  stylix.targets.kitty.enable = false;

  systemd.user.services.sops-fonts = {
    Unit = {
      Description = "Fonts with stupid licenses";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
    Service = {
      ExecStart = builtins.toString (
        pkgs.writeShellScript "install-fonts" ''
          #!/run/current-system/sw/bin/bash
          set -o errexit
          set -o nounset

          ${pkgs.coreutils}/bin/mkdir -p "${config.xdg.dataHome}/fonts"
          ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.monolisa.path}" -d "${config.xdg.dataHome}/fonts"
          ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.dankmono.path}" -d "${config.xdg.dataHome}/fonts"
          ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.operatormono.path}" -d "${config.xdg.dataHome}/fonts"
          ${pkgs.fontconfig}/bin/fc-cache -f
        ''
      );
    };
  };
}
