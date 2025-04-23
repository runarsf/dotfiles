{
  config,
  outputs,
  ...
}: let
  service = "beets";
  self = config.modules.services."${service}";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";
in
  outputs.lib.mkServiceModule config "${service}" {
    config = {
      home.activation."${service}" = ''
        mkdir -p ${base}/data \
                 ${media}/downloads \
                 ${media}/music
      '';

      nixos = {
        virtualisation.oci-containers.containers."${service}" = {
          image = "lscr.io/linuxserver/beets:latest";
          extraOptions = ["--pull=newer"];
          ports = ["8337:8337"];
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
          "beets.${self.domain}" = {
            forceSSL = true;
            sslCertificate = self.cert;
            sslCertificateKey = self.key;
            locations."/".proxyPass = "http://0.0.0.0:8337";
          };
        };
      };
    };
  }
