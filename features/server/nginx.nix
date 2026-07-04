_: {
  flake.nixosModules.nginx = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (builtins) listToAttrs concatMap;
    inherit (lib) mkIf mkOption optionalAttrs;
    inherit (lib.types) listOf str bool;
    inherit (lib.strings) concatMapStringsSep splitString;
    inherit (pkgs) fetchurl;

    cfg = config.features.nginx;
  in {
    options.features.nginx = {
      domains = mkOption {
        type = listOf str;
        default = [];
        description = "Base domains. Wildcard ACME certs are issued for each when non-empty.";
      };
      email = mkOption {
        type = str;
        description = "Contact email for ACME certificates.";
      };
      dyndns = mkOption {
        type = bool;
        default = true;
        description = "Whether to enable dynamic DNS.";
      };
    };

    config = mkIf (cfg.domains != []) {
      sops =
        {
          templates."acme-credentials".content = ''
            CLOUDFLARE_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_token}
          '';
        }
        // optionalAttrs cfg.dyndns {
          secrets.cloudflare_token = {};
          templates."cloudflare-dyndns".content = ''
            ${config.sops.placeholder.cloudflare_token}
          '';
        };

      services.nginx = {
        enable = true;
        # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/servers/http/nginx/modules.nix
        additionalModules = with pkgs.nginxModules; [pam dav];
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        # https://nixos.wiki/wiki/Nginx#Using_realIP_when_behind_CloudFlare_or_other_CDN
        commonHttpConfig = let
          realIpsFromList =
            concatMapStringsSep "\n"
            (x: "set_real_ip_from  ${x};");
          fileToList = x:
            splitString "\n" (builtins.readFile x);
          cfipv4 = fileToList (fetchurl {
            url = "https://www.cloudflare.com/ips-v4";
            sha256 = "0ywy9sg7spafi3gm9q5wb59lbiq0swvf0q3iazl0maq1pj1nsb7h";
          });
          cfipv6 = fileToList (fetchurl {
            url = "https://www.cloudflare.com/ips-v6";
            sha256 = "1ad09hijignj6zlqvdjxv7rjj8567z357zfavv201b9vx3ikk7cy";
          });
        in ''
          ${realIpsFromList cfipv4}
          ${realIpsFromList cfipv6}
          real_ip_header CF-Connecting-IP;
        '';

        appendHttpConfig = ''
          limit_req_zone $binary_remote_addr zone=mylimit:10m rate=10r/s;
        '';

        virtualHosts."_" = {
          forceSSL = true;
          sslCertificate = cfg.cert;
          sslCertificateKey = cfg.key;
          locations."/" = {return = "418";};
        };
      };

      networking.firewall.allowedTCPPorts = [80 443];
      users.groups.acmereceivers.members = ["nginx"];

      security.acme = {
        acceptTerms = true;
        defaults = {
          email = cfg.email;
          renewInterval = "*-*-* 00,12:00:00";
          reloadServices = ["nginx"];
        };
        certs = listToAttrs (map (domain: {
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

      services.cloudflare-dyndns = optionalAttrs cfg.dyndns {
        enable = cfg.dyndns;
        proxied = true;
        domains = concatMap (domain: ["*.${domain}" domain]) cfg.domains;
        apiTokenFile = config.sops.templates."cloudflare-dyndns".path;
      };
    };
  };
}
