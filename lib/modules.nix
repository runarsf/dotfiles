{ outputs, ... }:

rec {
  mkModuleWithOptions = { config, name, moduleConfig, default ? false
    , extraOptions ? { }, extraCondition ? true }:
    let
      namePathList = outputs.lib.splitString "." name;

      modulePath = [ "modules" ] ++ namePathList;
      enableOptionPath = modulePath ++ [ "enable" ];

      moduleOptions = {
        enable = outputs.lib.mkOption {
          inherit default;
          type = outputs.lib.types.bool;
          description = "Enable [${name}] module";
        };
      } // extraOptions;
    in {
      options = outputs.lib.setAttrByPath modulePath moduleOptions;

      config = outputs.lib.mkIf
        (outputs.lib.getAttrFromPath enableOptionPath config && extraCondition)
        moduleConfig;
    };

  mkModule' = config: name: extraOptions: moduleConfig:
    mkModuleWithOptions { inherit config name extraOptions moduleConfig; };

  mkModule = config: name: moduleConfig: mkModule' config name { } moduleConfig;

  mkEnabledModule' = config: name: extraOptions: moduleConfig:
    mkModuleWithOptions {
      inherit config name extraOptions moduleConfig;
      default = true;
    };

  mkEnabledModule = config: name: moduleConfig:
    mkEnabledModule' config name { } moduleConfig;

  mkDesktopModule' = config: name: extraOptions: moduleConfig:
    mkModuleWithOptions {
      inherit config name extraOptions moduleConfig;
      extraCondition = config.isDesktop;
    };

  mkDesktopModule = config: name: moduleConfig:
    mkDesktopModule' config name { } moduleConfig;
}
