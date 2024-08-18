{ outputs, ... }:

let
  sanitize = name: builtins.replaceStrings [ " " ] [ "-" ] (outputs.lib.toLower name);

in
rec {
  enable =
    elems:
    builtins.listToAttrs (
      map (name: {
        inherit name;
        value.enable = true;
      }) elems
    );

  disable =
    elems:
    builtins.listToAttrs (
      map (name: {
        name = name;
        value.enable = false;
      }) elems
    );

  enableIf = cond: elems: if cond then (enable elems) else (disable elems);

  fill =
    attr: value: elems:
    builtins.listToAttrs (
      map (name: {
        name = name;
        value."${attr}" = value;
      }) elems
    );

  mkModuleWithOptions =
    {
      config,
      name,
      result,
      default ? false,
      extraOptions ? { },
      extraCondition ? true,
    }:
    {
      options = outputs.lib.deepMerge [
        {
          # TODO the property name of the module should be nest-able
          #  e.g. modules.python.enable and modules.python.ide.enable
          modules.${name}.enable = outputs.lib.mkOption {
            inherit default;
            type = outputs.lib.types.bool;
            description = "Enable ${name} module";
          };
        }
        extraOptions
      ];
      config = outputs.lib.mkIf (config.modules.${name}.enable && extraCondition) result;
    };

  /*
    mkModuleWithOptions =
    {
      config,
      name,
      result,
      default ? false,
      extraOptions ? { },
    }:
    let
      parts = outputs.lib.splitString "." name;

      buildNestedOptions =
        parts: value:
        if builtins.length parts == 0 then
          value
        else
          let
            current = builtins.head parts;
            rest = builtins.tail parts;
          in
          {
            "${current}" = buildNestedOptions rest value;
          };

      options = buildNestedOptions parts {
        enable = outputs.lib.mkOption {
          inherit default;
          type = outputs.lib.types.bool;
          description = "Enable ${name} module";
        };
      };

      config = buildNestedOptions parts {
        enable = outputs.lib.mkIf config.modules.${name}.enable result;
      };
    in
    {
      inherit options config;
    };
  */

  mkModule' =
    config: name: extraOptions: result:
    mkModuleWithOptions {
      inherit
        config
        name
        result
        extraOptions
        ;
    };

  mkModule =
    config: name: result:
    mkModule' config name { } result;

  mkEnabledModule' =
    config: name: extraOptions: result:
    mkModuleWithOptions {
      inherit
        config
        name
        result
        extraOptions
        ;
      default = true;
    };

  mkEnabledModule =
    config: name: result:
    mkEnabledModule' config name { } result;

  mkDesktopModule' =
    config: name: extraOptions: result:
    mkModuleWithOptions {
      inherit
        config
        name
        result
        extraOptions
        ;
      extraCondition = outputs.lib.isDesktop' config;
    };

  mkDesktopModule =
    config: name: result:
    mkDesktopModule' config name { } result;
}
