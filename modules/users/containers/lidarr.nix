{ config, outputs, ... }:

let
  service = "lidarr";
  self = config.modules.services."${service}";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkServiceModule config "${service}" {
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
      "lidarr.${self.domain}" = {
        forceSSL = true;
        sslCertificate = self.cert;
        sslCertificateKey = self.key;
        locations."/".proxyPass = "http://0.0.0.0:8686";
      };
    };
  };
}
