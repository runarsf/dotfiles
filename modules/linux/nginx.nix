{ config, inputs, pkgs, domain, email, cert, key, ... }:

{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  users.groups.acmereceivers.members = [ "nginx" ];
  # systemd.user.services.nginx.Unit.After = [ "sops-nix.service" ];

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
      credentialsFile = config.sops.templates."acme-credentials".path;
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

  # This converts your ssh key to an age key during every build,
  # puts it in $XDG_RUNTIME_DIR/secrets.d/age-keys.txt,
  # and points age.keyFile to the generated age key.
  sops.age.sshKeyPaths = [ "/home/runar/.ssh/nix" ];

  sops = {
    defaultSopsFile = "${inputs.vault}/secrets.yaml";
  };

  services.cloudflare-dyndns = {
    enable = true;
    proxied = true;
    domains = [ "*.${domain}" "${domain}" ];
    apiTokenFile = config.sops.templates."cloudflare-dyndns".path;
  };

  sops.secrets.cloudflare_token = {};
  sops.templates."acme-credentials".content = ''
    CLOUDFLARE_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_token}
  '';
  sops.templates."cloudflare-dyndns".content = ''
    CLOUDFLARE_API_TOKEN=${config.sops.placeholder.cloudflare_token}
  '';
} 
