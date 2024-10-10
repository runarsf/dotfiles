{ config, outputs, ... }:

# https://github.com/awesome-selfhosted/awesome-selfhosted?tab=readme-ov-file#file-transfer---web-based-file-managers
# https://github.com/filebrowser/filebrowser
# https://docs.filegator.io/install.html

let
  # TODO Where tf should this *actually* be stored? /var/lib?
  service = "gonic";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkModule config "${service}" {
  nixos = {
    system.userActivationScripts."${service}".text = ''
      mkdir -p ${base}/data \
               ${base}/playlists \
               ${base}/cache \
               ${media}/podcasts \
               ${media}/music
    '';

    virtualisation.oci-containers.containers."${service}" = {
      image = "sentriz/gonic:latest";
      extraOptions =
        [ "--pull=newer" "--group-add=audio" "--device=/dev/snd:/dev/snd" ];
      ports = [ "4747:80" ];
      environment = {
        TZ = "Europe/Oslo";
        GONIC_JUKEBOX_ENABLED = "true";
      };
      volumes = [
        "${base}/data:/data"
        "${base}/playlists:/playlists"
        "${base}/cache:/cache"
        "${media}/music:/music:ro"
        "${media}/podcasts:/podcasts"
      ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "fm.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/".proxyPass = "http://0.0.0.0:4747";
      };
    };
  };
}

