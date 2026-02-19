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

  mkContainer = config: containerConfig:
    outputs.lib.deepMerge [
      {
        extraPodmanArgs = [
          "--pull=newer"
          "--group-add=keep-groups"
          # ''--gidmap="+g102000:@2000"''
        ];
        autoUpdate = "registry";
        userNS = outputs.lib.mkDefault "keep-id";
        network = "pasta";
        # user = config.nixos.users.users."${name}".uid;
        # group = config.nixos.users.groups."${name}".gid;
        # userNS = ''keep-id:uid=${builtins.toString config.nixos.users.users."${name}".uid},gid=${builtins.toString config.nixos.users.groups."${name}".gid}'';
        # environment = {
        # PUID = config.nixos.users.users."${config.home.username}".uid;
        # PGID = config.nixos.users.groups."${config.home.username}".gid;
        # UMASK = "002";
        # };
      }
      containerConfig
    ];
  # // (containerConfig
  #   // outputs.lib.optionals ((containerConfig.environmentFiles or null) != null) {
  #     environmentFiles =
  #       map (
  #         item:
  #           if builtins.isString item
  #           then config.sops.templates."${item}".path
  #           else item
  #       )
  #       containerConfig.environmentFiles;
  #   });
}
