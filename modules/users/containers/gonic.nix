{
  config,
  outputs,
  ...
}: let
  service = "gonic";
  self = config.modules.services."${service}";
  # TODO Where tf should this *actually* be stored? /var/lib?
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";
in
  outputs.lib.mkServiceModule config "${service}" {
    config = {
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
          extraOptions = ["--pull=newer" "--group-add=audio" "--device=/dev/snd:/dev/snd"];
          ports = ["4747:80"];
          # https://github.com/sentriz/gonic?tab=readme-ov-file#configuration-options
          environment = let
            multi = "delim ;";
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

        services.nginx.virtualHosts = {
          "fm.${self.domain}" = {
            forceSSL = true;
            sslCertificate = self.cert;
            sslCertificateKey = self.key;
            locations."/".proxyPass = "http://0.0.0.0:4747";
          };
        };
      };
    };
  }
