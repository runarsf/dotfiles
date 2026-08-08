_: {
  flake.homeModules.fonts = {
    config,
    osConfig ? {},
    pkgs,
    self,
    lib,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) system;

    vaultPath = osConfig.features.sops.vaultPath or null;
  in
    lib.mkMerge [
      {
        home.packages = [self.packages.${system}.scientifica-hidpi];
      }
      (lib.mkIf (vaultPath != null) {
        sops.secrets = {
          monolisa = {
            sopsFile = "${vaultPath}/shared/fonts/MonoLisa.zip";
            format = "binary";
          };
          dankmono = {
            sopsFile = "${vaultPath}/shared/fonts/DankMono.zip";
            format = "binary";
          };
          operatormono = {
            sopsFile = "${vaultPath}/shared/fonts/OperatorMono.zip";
            format = "binary";
          };
        };

        systemd.user.services.sops-fonts = {
          Unit = {
            Description = "Fonts with stupid licenses";
            PartOf = ["home-manager-${config.home.username}.service"];
          };
          Install.WantedBy = ["default.target"];
          Service.ExecStart = toString (pkgs.writeShellScript "install-fonts" ''
            set -o errexit
            set -o nounset

            ${pkgs.coreutils}/bin/mkdir -p "${config.xdg.dataHome}/fonts"
            ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.monolisa.path}" -d "${config.xdg.dataHome}/fonts"
            ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.dankmono.path}" -d "${config.xdg.dataHome}/fonts"
            ${pkgs.unzip}/bin/unzip -o "${config.sops.secrets.operatormono.path}" -d "${config.xdg.dataHome}/fonts"
            ${pkgs.fontconfig}/bin/fc-cache -f
          '');
        };
      })
    ];
}
