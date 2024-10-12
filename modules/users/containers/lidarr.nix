{ config, outputs, ... }:

let
  service = "lidarr";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkModule config "${service}" {
  home.activation."${service}" = ''
    mkdir -p ${base}/data \
             ${media}/downloads \
             ${media}/music
  '';

  nixos = {
    virtualisation.oci-containers.containers."${service}" = {
      image = "lscr.io/linuxserver/lidarr:latest";
      extraOptions = [ "--pull=newer" ];
      ports = [ "8686:8686" ];
      environment = {
        TZ = "Europe/Oslo";
        PUID = "1000";
        PGID = "1000";
      };
      volumes = [
        "${base}/data:/config"
        "${media}/music:/music"
        "${media}/downloads:/downloads"
      ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "lidarr.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/".proxyPass = "http://0.0.0.0:8686";
      };
    };
  };
}

