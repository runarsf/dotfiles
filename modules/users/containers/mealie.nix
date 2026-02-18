{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config ["containers" "mealie"] {
  services.podman.containers = {
    "mealie" = outputs.lib.mkContainer config {
      image = "ghcr.io/mealie-recipes/mealie:v3.9.2";
      ports = [
        "9925:9000" # http
      ];
      volumes = with config.modules.containers.dirs; [
        "${containers}/mealie/data:/app/data"
      ];
      environment = {
        TZ = "Europe/Oslo";
        PGID = config.nixos.users.groups."${name}".gid;
        PUID = config.nixos.users.users."${name}".uid;
        ALLOW_SIGNUP = "false";
        BASE_URL = "https://recipes.${config.modules.nginx.domain}";
        # FRESHRSS_INSTALL = "--api-enabled --base-url=https://rss.${config.modules.nginx.domain} --default-user=${config.modules.containers.username} --language=en";
      };
      # environmentFile = [
      #   config.sops.templates."freshrss-env".path
      # ];
      # userNS = "keep-id:uid=0,gid=0";
    };
  };

  nixos = {
    services.nginx.virtualHosts = {
      "recipes.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "http://127.0.0.1:9925";
        };
      };
    };
  };
}
