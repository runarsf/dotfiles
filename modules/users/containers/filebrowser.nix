{ config, outputs, ... }:

let
  service = "filebrowser";
  self = config.modules.services."${service}";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkServiceModule config "${service}" {
  home.activation."${service}" = ''
    mkdir -p ${base} \
             ${media}/music
    touch ${base}/filebrowser.db
  '';

  nixos = {
      # system.userActivationScripts."${service}".text = ''
      #   mkdir -p ${media}/music
      #   touch ${base}/filebrowser.db
      # '';

    virtualisation.oci-containers.containers."${service}" = {
      image = "filebrowser/filebrowser:latest";
      extraOptions = [ "--pull=newer" ];
      ports = [ "4113:80" ];
      volumes = [ "${media}/music:/srv" "${base}/filebrowser.db:/database.db" ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "fs.${self.domain}" = {
        forceSSL = true;
        sslCertificate = self.cert;
        sslCertificateKey = self.key;
        locations."/".proxyPass = "http://0.0.0.0:4113";
      };
    };
  };
}
