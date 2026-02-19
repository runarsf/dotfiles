{
  config,
  pkgs,
  outputs,
  ...
}: let
  cfg = config.modules.matrix;
in
  outputs.lib.mkDesktopModule config "matrix" {
    options' = {
      clients = with outputs.lib; {
        element.enable = mkEnableOption "Enable Element Desktop";
        cinny.enable = mkEnableOption "Enable Cinny";
        commet.enable = mkEnableOption "Enable Commet";
      };
    };

    config = {
      programs.element-desktop.enable = cfg.clients.element.enable;

      nixos.services.flatpak = {
        packages = with outputs.lib;
          optionals cfg.clients.cinny.enable (trace "When using Cinny, make sure you also have flatpak enabled." [
            "in.cinny.Cinny"
          ])
          ++ optionals cfg.clients.commet.enable (trace "When using Commet, make sure you also have flatpak enabled." [
            rec {
              appId = "chat.commet.commetapp";
              sha256 = "sha256-NBZIyGGlg9MHkXCcDwGRnJIjv3EsbAHdnp01C/B8zHQ=";
              bundle = "${pkgs.fetchurl {
                url = "https://github.com/commetchat/commet/releases/download/v0.4.0/chat.commet.commetapp.flatpak";
                inherit sha256;
              }}";
            }
          ]);
      };
    };
  }
