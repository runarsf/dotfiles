{ domain, cert, key, email, pkgs, ... }:

{
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  networking.firewall = {
    allowedTCPPorts = [ 80 443 ];
  };

  users.groups.acmereceivers = { members = [ "nginx" ]; };

  security.acme = {
    defaults = {
      renewInterval = "*-*-* 00,12:00:00";
      reloadServices = [ "nginx" ];
    };
    acceptTerms = true;
    defaults.email = email;
    certs."${domain}" = {
      domain = "*.${domain}";
      extraDomainNames = [ domain ];
      group = "acmereceivers";
      dnsProvider = "cloudflare";
      # TODO Use sops key
      credentialsFile = "${pkgs.writeText "cloudflare-credentials" ''
        CLOUDFLARE_DNS_API_TOKEN=
      ''}";
    };
  };

  services.nginx.virtualHosts = {
    "_" = {
      forceSSL = true;
      sslCertificate = cert;
      sslCertificateKey = key;
      locations."/" = { return = "418"; };
    };
  };

  # https://dash.cloudflare.com/profile/api-tokens
  #  #dns_records:edit
  #  #zone:read
  # ??? https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/misc/cfdyndns.nix
  # virtualisation.oci-containers.containers.cloudflare-ddns = {
  #   image = "joshava/cloudflare-ddns:latest";
  #   autoStart = true;
  #   environment = {
  #     PUID = "1000";
  #     GUID = "1000";
  #     CRON = "0 */12 * * *";
  #     ZONE = domain;
  #     HOST = "*.${domain},${domain}";
  #     EMAIL = email;
  #     API = "";
  #   };
  #   # FIXME
  #   volumes = [ "${../../. + builtins.toPath "/cloudflare-ddns.yaml"}:/app/config.yaml" ];
  # };
}
