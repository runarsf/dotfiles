{ outputs, ... }:

{
  options = {
    system = outputs.lib.mkOption {
      type = outputs.lib.types.deepMergedAttrs;
      default = { };

      description = ''
        Adds support to define additional NixOS system configurations.
        Without this module, home-manager will complain about the "system" option not existing.

        TODO: Document how it should be NixOS modules and the corresponding lib function.
      '';
    };
  };
}
