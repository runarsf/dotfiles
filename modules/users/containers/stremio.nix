{
  config,
  outputs,
  ...
}:
outputs.lib.mkModule config ["services" "stremio"] {
  config = {
    nixos = {
      virtualisation.oci-containers.containers."stremio" = {
        image = "stremio/server:latest";
        extraOptions = ["--pull=newer"];
        ports = ["11470:11470" "12470:12470"];
        environment = {
          NO_CORS = "1";
        };
      };

      networking.firewall.allowedTCPPorts = [11470 12470];
      networking.firewall.allowedUDPPorts = [11470 12470];

      services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
        "stream.${config.modules.nginx.domain}" = {
          forceSSL = true;
          sslCertificate = config.modules.nginx.cert;
          sslCertificateKey = config.modules.nginx.key;
          locations."/" = {
            proxyPass = "http://0.0.0.0:11470";
            proxyWebsockets = true;
          };
        };
      };
    };
  };
}
