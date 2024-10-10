{ config, outputs, ... }:

{
  options = {
    isDesktop = outputs.lib.mkOption {
      default = config.home.sessionVariables ? NIXOS_OZONE_WL;
      type = outputs.lib.types.bool;
    };
  };
}
