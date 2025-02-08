{ config, outputs, ... }:

{
  options = with outputs.lib; {
    isDesktop = mkOption {
      default = config.home.sessionVariables ? NIXOS_OZONE_WL;
      type = types.bool;
    };
    defaultTerminal = mkOption {
      default =
        if (config.isDesktop) then (throw "defaultTerminal not set") else null;
      type = types.nullOr types.str;
    };
    defaultBrowser = mkOption {
      default =
        if (config.isDesktop) then (throw "defaultBrowser not set") else null;
      type = types.nullOr types.str;
    };
    avatar = mkOption {
      type = types.path;
    };
    PATH = mkOption {
      default = [];
      type = types.listOf types.str;
    };
  };

  config = outputs.lib.deepMerge [
    { home.sessionVariables.PATH = outputs.lib.concatStringsSep ":" (config.PATH ++ [ "\${PATH}" ]); }
    (outputs.lib.mkIf (config.isDesktop && config.defaultTerminal != null) {
      home.sessionVariables.TERMINAL = config.defaultTerminal;

      modules."${config.defaultTerminal}".enable = true;
    })
    (outputs.lib.mkIf (config.isDesktop && config.defaultBrowser != null) {
      modules."${config.defaultBrowser}".enable = true;
    })
  ];
}

