{outputs, ...}: rec {
  mkDesktopModule' = config: name: default: moduleConfig:
    outputs.lib.mkModuleWithOptions {
      inherit config name default moduleConfig;
      extraCondition = x: x && config.isDesktop;
    };

  mkDesktopModule = config: name: moduleConfig:
    mkDesktopModule' config name false moduleConfig;

  mkServiceModule' = config: name: default: moduleConfig: let
    serviceName = ["services"] ++ outputs.lib.lists.toList name;
    domain =
      outputs.lib.getAttrFromPath (["modules"] ++ serviceName ++ ["domain"]) config;
  in
    outputs.lib.mkModuleWithOptions {
      inherit config default;
      name = serviceName;
      # Defines overrides for nginx-related properties
      moduleConfig = {
        options' =
          {
            domain = outputs.lib.mkOption {
              default = builtins.head config.modules.nginx.domains;
              type = outputs.lib.types.str;
              description = "Domain for [${name}] service";
            };
            cert = outputs.lib.mkOption {
              default = "/var/lib/acme/${domain}/cert.pem";
              type = outputs.lib.types.str;
              description = "Certificate for [${name}] service";
            };
            key = outputs.lib.mkOption {
              default = "/var/lib/acme/${domain}/key.pem";
              type = outputs.lib.types.str;
              description = "Certificate for [${name}] service";
            };
          }
          // moduleConfig.options' or {};
        options = moduleConfig.options or {};

        config =
          if (moduleConfig ? options || moduleConfig ? options')
          then moduleConfig.config or (throw "Missing toplevel config attribute")
          else moduleConfig.config or moduleConfig;
      };
    };

  mkServiceModule = config: name: moduleConfig:
    mkServiceModule' config name false moduleConfig;
}
