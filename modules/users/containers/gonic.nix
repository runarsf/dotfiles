{ config, outputs, ... }:

# https://github.com/awesome-selfhosted/awesome-selfhosted?tab=readme-ov-file#file-transfer---web-based-file-managers
# https://github.com/filebrowser/filebrowser
# https://docs.filegator.io/install.html

let
  # TODO Where tf should this *actually* be stored? /var/lib?
  service = "gonic";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkServiceModule config "${service}" {
  home.activation."${service}" = ''
    mkdir -p ${base}/data \
             ${base}/playlists \
             ${base}/cache \
             ${media}/podcasts \
             ${media}/music
  '';

  nixos = {
    virtualisation.oci-containers.containers."${service}" = {
      image = "sentriz/gonic:latest";
      extraOptions =
        [ "--pull=newer" "--group-add=audio" "--device=/dev/snd:/dev/snd" ];
      ports = [ "4747:80" ];
      # https://github.com/sentriz/gonic?tab=readme-ov-file#configuration-options
      environment = let multi = "delim ;";
      in {
        TZ = "Europe/Oslo";
        GONIC_JUKEBOX_ENABLED = "true";
        # GONIC_MUSIC_PATH = "";
        GONIC_MULTI_VALUE_GENRE = multi;
        GONIC_MULTI_VALUE_ARTIST = multi;
        GONIC_MULTI_VALUE_ALBUM_ARTIST = multi;
        GONIC_SCAN_WATCHER_ENABLED = "true";
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
      "fm.${config.modules.services.gonic.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/".proxyPass = "http://0.0.0.0:4747";
      };
    };
  };
}

