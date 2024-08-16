{ outputs, ... }:

rec {
  enable =
    elems:
    builtins.listToAttrs (
      map (name: {
        name = name;
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
    }:
    let
      mname = builtins.replaceStrings [ " " ] [ "-" ] (outputs.lib.toLower name);
    in
    {
      options = outputs.lib.deepMerge [
        {
          modules.${mname}.enable = outputs.lib.mkOption {
            inherit default;
            type = outputs.lib.types.bool;
            description = "Enable ${name} module";
          };
        }
        extraOptions
      ];
      config = outputs.lib.mkIf config.modules.${mname}.enable result;
    };
 

  /* mkModuleWithOptions =
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
    }; */

  mkModule =
    config: name: result:
    mkModuleWithOptions { inherit config name result; };

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

  mkEnabledModule =
    config: name: result:
    mkModuleWithOptions {
      inherit config name result;
      default = true;
    };

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

  # TODO mkDesktopModule should have a second "master" option for enabling/disabling.
  #  e.g. modules.discord.enable and modules.discord.onDesktop, where both need to be true.
  mkDesktopModule =
    config: name: result:
    mkModuleWithOptions {
      inherit config name result;
      default = outputs.lib.isDesktop' config;
    };

  mkDesktopModule' =
    config: name: extraOptions: result:
    mkModuleWithOptions {
      inherit
        config
        name
        result
        extraOptions
        ;
      default = outputs.lib.isDesktop' config;
    };
}
