{ config, outputs, osConfig, ... }:

outputs.lib.mkModule' config "nginx" {
  modules.nginx = {
    domains = outputs.lib.mkOption {
      type = outputs.lib.types.listOf outputs.lib.types.str;
    };
    domain = outputs.lib.mkOption {
      type = outputs.lib.types.str;
      default = builtins.head config.modules.nginx.domains;
      defaultText = "First domain in the list";
    };
    email = outputs.lib.mkOption { type = outputs.lib.types.str; };
    cert = outputs.lib.mkOption {
      type = outputs.lib.types.str;
      default =
        "/var/lib/acme/${config.modules.nginx.domain}/cert.pem";
    };
    key = outputs.lib.mkOption {
      type = outputs.lib.types.str;
      default =
        "/var/lib/acme/${config.modules.nginx.domain}/key.pem";
    };
  };
} {
  nixos = {
    # TODO Migrate to hm sops-nix when templates are supported https://github.com/Mic92/sops-nix/issues/423
    sops = {
      secrets.cloudflare_token = { };
      templates."acme-credentials".content = ''
        CLOUDFLARE_DNS_API_TOKEN=${osConfig.sops.placeholder.cloudflare_token}
      '';
      templates."cloudflare-dyndns".content = ''
        CLOUDFLARE_API_TOKEN=${osConfig.sops.placeholder.cloudflare_token}
      '';
    };

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
    users.groups.acmereceivers.members = [ "nginx" ];

    security.acme = {
      defaults = {
        renewInterval = "*-*-* 00,12:00:00";
        reloadServices = [ "nginx" ];
      };
      acceptTerms = true;
      defaults.email = config.modules.nginx.email;
      certs = builtins.listToAttrs (map (domain: {
        name = domain;
        value = {
          domain = "*.${domain}";
          extraDomainNames = [ domain ];
          group = "acmereceivers";
          dnsProvider = "cloudflare";
          credentialsFile = osConfig.sops.templates."acme-credentials".path;
        };
      }) config.modules.nginx.domains);
    };

    services.nginx.virtualHosts = {
      "_" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = { return = "418"; };
      };
    };

    services.cloudflare-dyndns = {
      enable = true;
      proxied = true;
      domains = builtins.concatMap (domain: [ "*.${domain}" domain ])
        config.modules.nginx.domains;
      apiTokenFile = osConfig.sops.templates."cloudflare-dyndns".path;
    };
  };
}
