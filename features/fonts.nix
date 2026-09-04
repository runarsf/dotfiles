{ lib', ... }: {
  flake.nixosModules.fonts =
    { config, lib, ... }:
    let
      inherit (lib) mkEnableOption mkIf;

      cfg = config.features.fonts;
    in
    {
      options.features.fonts = {
        oled = mkEnableOption "OLED";
      };

      config = {
        fonts.fontconfig = {
          enable = true;
          antialias = true;
          hinting = {
            enable = true;
            style = "slight";
          };
          subpixel.rgba = if cfg.oled then "none" else "rgb";
        };

        environment.variables.FREETYPE_PROPERTIES = mkIf cfg.oled "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0 type1:no-stem-darkening=0";
      };
    };

  flake.homeModules.fonts =
    {
      config,
      osConfig ? { },
      pkgs,
      self,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      inherit (lib'.fonts) nofontsdir;

      vaultPath = osConfig.features.sops.vaultPath or null;
    in
    lib.mkMerge [
      {
        fonts.fontconfig.enable = true;

        home.packages =
          with pkgs;
          [
            # Writing
            libertine
            libertinus
            atkinson-hyperlegible
            montserrat
            roboto
            ia-writer-duospace
            inter

            # Unicode table
            noto-fonts
            noto-fonts-color-emoji
            noto-fonts-cjk-sans
            powerline-fonts

            # Bitmap fonts
            self.packages.${system}.scientifica-hidpi
            creep
            cozette
            undefined-medium
            zpix-pixel-font
            termsyn
            terminus_font
            monocraft
            departure-mono
            unifont
            (nofontsdir unifont_upper)
            (nofontsdir tamzen)
            (nofontsdir proggyfonts)
            (nofontsdir gohufont)
            (nofontsdir spleen)

            # Coding
            jetbrains-mono
            sudo-font
            cascadia-code
            # maple-mono.NF
            mplus-outline-fonts.githubRelease
          ]
          ++ (with pkgs.nerd-fonts; [
            caskaydia-cove
            caskaydia-mono
            comic-shanns-mono
            jetbrains-mono
            monaspace
            ubuntu
            ubuntu-mono
            commit-mono
            im-writing
            fira-code
            gohufont
            lilex
            departure-mono
            noto
          ]);
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
            PartOf = [ "home-manager-${config.home.username}.service" ];
          };
          Install.WantedBy = [ "default.target" ];
          Service.ExecStart = toString (
            pkgs.writeShellScript "install-fonts" ''
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
      })
    ];
}
