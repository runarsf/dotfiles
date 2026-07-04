{self, lib', ...}: {
  flake.nixosModules.teleport = {
    config,
    lib,
    ...
  }: let
    cfg = config.features.teleport;
    nginxDomains = config.features.nginx.domains;
    usingNginx = nginxDomains != [];
    baseDomain = lib'.nginx.baseDomain nginxDomains cfg.domain;
  in {
    imports = [self.nixosModules.nginx];

    options.features.teleport.domain = lib.mkOption {
      type = lib.types.str;
      default =
        if nginxDomains != []
        then "tp.${builtins.head nginxDomains}"
        else throw "Set features.teleport.domain or configure features.nginx.domains";
      description = "Public domain for the Teleport proxy.";
    };

    config = lib.mkMerge [
      {
        services.teleport = {
          enable = true;
          settings = {
            proxy_service = {
              enabled = true;
              web_listen_addr = "0.0.0.0:3080";
              public_addr = "${cfg.domain}:443";
              tunnel_listen_addr = "0.0.0.0:3024";
            };
            auth_service = {
              enabled = true;
              listen_addr = "0.0.0.0:3025";
            };
            ssh_service = {
              enabled = true;
              listen_addr = "0.0.0.0:3022";
            };
          };
        };
      }

      (lib.mkIf usingNginx {
        services.nginx.virtualHosts.${cfg.domain} = lib'.nginx.wildcardVhost baseDomain // {
          locations."/" = {
            proxyPass = "https://127.0.0.1:3080";
            proxyWebsockets = true;
            extraConfig = "proxy_set_header Host $host;";
          };
        };
      })

      (lib.mkIf (!usingNginx) {
        security.acme = {
          acceptTerms = true;
          certs.${cfg.domain} = {};
        };
      })
    ];
  };
}
