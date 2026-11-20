{ lib, lib', ... }:
{
  # A container-backed service behind nginx: declares `modules.<name>.domain`,
  # a vhost proxying to `port` under the base domain's wildcard cert, and pulls
  # in the nginx and containers modules. `module` holds the service itself --
  # its containers, secrets, and any extra vhost settings, which merge with the
  # vhost made here. Goes through mkModule, so one service can check another
  # with `config.modules.<name>.enable or false`.
  mkService =
    {
      self,
      name,
      port,
      subdomain ? name,
      proxyHost ? "127.0.0.1",
      websockets ? false,
      maxBodySize ? null,
      module ? _: { },
    }:
    lib'.mkModule name (
      { config, ... }:
      let
        cfg = config.modules.${name};
        inherit (config.modules.nginx) domains;
      in
      {
        imports = [
          (lib'.importModule self "nixos" "nginx")
          (lib'.importModule self "nixos" "containers")
          module
        ];

        options.modules.${name}.domain = lib.mkOption {
          type = lib.types.str;
          default =
            if domains != [ ] then
              "${subdomain}.${builtins.head domains}"
            else
              throw "Set modules.${name}.domain or configure modules.nginx.domains";
          description = "Public domain for the ${name} service.";
        };

        config = lib.mkIf (domains != [ ]) {
          services.nginx.virtualHosts.${cfg.domain} =
            lib'.nginx.wildcardVhost (lib'.nginx.baseDomain domains cfg.domain)
            // {
              locations."/" = {
                proxyPass = "http://${proxyHost}:${toString port}";
                proxyWebsockets = websockets;
              };
            }
            // lib.optionalAttrs (maxBodySize != null) {
              extraConfig = "client_max_body_size ${maxBodySize};";
            };
        };
      }
    );
}
