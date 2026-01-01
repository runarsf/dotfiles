{
  config,
  pkgs,
  outputs,
  ...
}: let
  cfg = config.modules.nginx;
in
  # TODO mkServiceModule that lets you define domain
  outputs.lib.mkModule config "nginx" {
    options' = with outputs.lib;
    with types; {
      email = mkOption {
        type = str;
      };
      domains = mkOption {
        type = listOf str;
      };
      domain = mkOption {
        default = builtins.head cfg.domains;
        type = str;
      };
      cert = outputs.lib.mkOption {
        default = "/var/lib/acme/${cfg.domain}/cert.pem";
        type = str;
      };
      key = outputs.lib.mkOption {
        default = "/var/lib/acme/${cfg.domain}/key.pem";
        type = str;
      };
    };

    config = {
      sops = {
        secrets.cloudflare_token = {};
        templates."acme-credentials".content = ''
          CLOUDFLARE_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_token}
        '';
        templates."cloudflare-dyndns".content = ''
          ${config.sops.placeholder.cloudflare_token}
        '';
      };
      nixos = {
        # systemd.services.nginx.serviceConfig.ReadWritePaths = ["/var/spool/nginx/logs/"];

        services.nginx = {
          enable = true;
          # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/servers/http/nginx/modules.nix
          additionalModules = with pkgs.nginxModules; [pam dav];
          recommendedGzipSettings = true;
          recommendedOptimisation = true;
          recommendedProxySettings = true;
          recommendedTlsSettings = true;

          # TODO https://nixos.wiki/wiki/Nginx#Hardened_setup_with_TLS_and_HSTS_preloading

          # https://nixos.wiki/wiki/Nginx#Using_realIP_when_behind_CloudFlare_or_other_CDN
          commonHttpConfig = let
            realIpsFromList =
              outputs.lib.strings.concatMapStringsSep "\n"
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

        networking.firewall.allowedTCPPorts = [80 443];
        users.groups.acmereceivers.members = ["nginx"];

        security.acme = {
          defaults = {
            renewInterval = "*-*-* 00,12:00:00";
            reloadServices = ["nginx"];
          };
          acceptTerms = true;
          defaults.email = cfg.email;
          certs = builtins.listToAttrs (map (domain: {
              name = domain;
              value = {
                domain = "*.${domain}";
                extraDomainNames = [domain];
                group = "acmereceivers";
                dnsProvider = "cloudflare";
                credentialsFile = config.sops.templates."acme-credentials".path;
              };
            })
            cfg.domains);
        };

        services.nginx.virtualHosts = {
          "_" = {
            forceSSL = true;
            sslCertificate = cfg.cert;
            sslCertificateKey = cfg.key;
            locations."/" = {return = "418";};
          };
        };

        services.cloudflare-dyndns = {
          enable = true;
          proxied = true;
          domains =
            builtins.concatMap (domain: ["*.${domain}" domain])
            cfg.domains;
          apiTokenFile = config.sops.templates."cloudflare-dyndns".path;
        };
      };
    };
  }
