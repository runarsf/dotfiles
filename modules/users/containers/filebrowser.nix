{ config, outputs, ... }:

let
  service = "filebrowser";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkModule config "${service}" {
  nixos = {
    system.userActivationScripts."${service}".text = ''
      mkdir -p ${media}/music
    '';

    virtualisation.oci-containers.containers."${service}" = {
      image = "filebrowser/filebrowser:latest";
      extraOptions = [ "--pull=newer" ];
      ports = [ "4113:80" ];
      volumes = [ "${media}/music:/srv" "${base}/filebrowser.db:/database.db" ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "files.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/".proxyPass = "http://0.0.0.0:4113";
      };
    };
  };
}

