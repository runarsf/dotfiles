{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config ["containers" "stremio"] {
  services.podman.containers = {
    "stremio" = outputs.lib.mkContainer config {
      image = "docker.io/stremio/server:latest";
      ports = [
        "11470:11470" # http
        "12470:12470" # https
      ];
      volumes = with config.modules.containers.dirs; [
        # curl http://127.0.0.1:11470/get-https?ipAddress=127.0.0.1
        "${containers}/stremio/data:/stremio/.stremio-server"
      ];
      environment = {
        NO_CORS = "1";
      };
    };
  };

  nixos = {
    services.nginx.virtualHosts = {
      "stream.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "https://127.0.0.1:12470";
          proxyWebsockets = true;
        };
      };
    };
  };
}
