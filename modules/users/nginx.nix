{ config, pkgs, outputs, osConfig, ... }:

# TODO mkServiceModule that lets you define domain

outputs.lib.mkModule' config "nginx" {
  email = outputs.lib.mkOption { type = outputs.lib.types.str; };
  domains = outputs.lib.mkOption {
    type = outputs.lib.types.listOf outputs.lib.types.str;
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
    systemd.services.nginx.serviceConfig.ReadWritePaths = [ "/var/spool/nginx/logs/" ];

    services.nginx = {
      enable = true;
      # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/servers/http/nginx/modules.nix
      additionalModules = with pkgs.nginxModules; [ pam dav ];
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      # TODO https://nixos.wiki/wiki/Nginx#Hardened_setup_with_TLS_and_HSTS_preloading

      # https://nixos.wiki/wiki/Nginx#Using_realIP_when_behind_CloudFlare_or_other_CDN
      commonHttpConfig = let
        realIpsFromList = outputs.lib.strings.concatMapStringsSep "\n"
          (x: "set_real_ip_from  ${x};");
        fileToList = x:
          outputs.lib.strings.splitString "\n" (builtins.readFile x);
        cfipv4 = fileToList (pkgs.fetchurl {
          url = "https://www.cloudflare.com/ips-v4";
          sha256 = "0ywy9sg7spafi3gm9q5wb59lbiq0swvf0q3iazl0maq1pj1nsb7h";
        });
        cfipv6 = fileToList (pkgs.fetchurl {
          url = "https://www.cloudflare.com/ips-v6";
          sha256 = "1ad09hijignj6zlqvdjxv7rjj8567z357zfavv201b9vx3ikk7cy";
        });
      in ''
        ${realIpsFromList cfipv4}
        ${realIpsFromList cfipv6}
        real_ip_header CF-Connecting-IP;
      '';
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

    services.cloudflare-dyndns = {
      enable = true;
      proxied = true;
      domains = builtins.concatMap (domain: [ "*.${domain}" domain ])
        config.modules.nginx.domains;
      apiTokenFile = osConfig.sops.templates."cloudflare-dyndns".path;
    };
  };
}
