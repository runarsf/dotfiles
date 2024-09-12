{ config, outputs, ... }:

{
  options = {
    defaultTerminal = outputs.lib.mkOption {
      default = throw "defaultTerminal not set";
      type = outputs.lib.types.str;
    };
  };

  config = outputs.lib.mkIf (outputs.lib.isDesktop' config) {
    home.sessionVariables.TERMINAL = config.defaultTerminal;
  };
}
