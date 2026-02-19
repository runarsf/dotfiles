{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config ["containers" "freshrss"] {
  # sops = {
  #   secrets = {
  #     freshrss_password = {};
  #     freshrss_api_password = {};
  #   };
  #   templates."freshrss-env".content = ''
  #     FRESHRSS_USER="--api-password=${config.sops.placeholder.freshrss_api_password} --email=${config.modules.nginx.email} --language=en --password=${config.sops.placeholder.freshrss_password} --user=${config.modules.containers.username}"
  #   '';
  # };

  services.podman.containers = {
    "freshrss" = outputs.lib.mkContainer config {
      image = "docker.io/freshrss/freshrss:latest";
      ports = [
        "4655:80" # http
      ];
      volumes = with config.modules.containers.dirs; [
        "${containers}/freshrss/data:/var/www/FreshRSS/data"
        "${containers}/freshrss/extensions:/var/www/FreshRSS/extensions"
      ];
      environment = {
        TZ = "Europe/Oslo";
        CRON_MIN = "2,32";
        # FRESHRSS_INSTALL = "--api-enabled --base-url=https://rss.${config.modules.nginx.domain} --default-user=${config.modules.containers.username} --language=en";
      };
      # environmentFile = [
      #   config.sops.templates."freshrss-env".path
      # ];
      userNS = "keep-id:uid=0,gid=0";
    };
  };

  nixos = {
    services.nginx.virtualHosts = {
      "rss.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "http://127.0.0.1:4655";
        };
      };
    };
  };
}
