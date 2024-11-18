{ config, outputs, ... }:

{
  options = {
    defaultTerminal = outputs.lib.mkOption {
      default =
        if (config.isDesktop) then (throw "defaultTerminal not set") else null;
      type = outputs.lib.types.nullOr outputs.lib.types.str;
    };
    defaultBrowser = outputs.lib.mkOption {
      default =
        if (config.isDesktop) then (throw "defaultBrowser not set") else null;
      type = outputs.lib.types.nullOr outputs.lib.types.str;
    };
    # TODO find a better place for this
    avatar = outputs.lib.mkOption {
      type = outputs.lib.types.path;
    };
  };

  config = outputs.lib.deepMerge [
    (outputs.lib.mkIf (config.isDesktop && config.defaultTerminal != null) {
      home.sessionVariables.TERMINAL = config.defaultTerminal;

      modules."${config.defaultTerminal}".enable = true;
    })
    (outputs.lib.mkIf (config.isDesktop && config.defaultBrowser != null) {
      modules."${config.defaultBrowser}".enable = true;
    })
  ];
}
