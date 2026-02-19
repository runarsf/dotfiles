{
  config,
  outputs,
  name,
  ...
}: let
  data = "${config.home.homeDirectory}/data/containers/slskd";
  media = "${config.home.homeDirectory}/data/media";
in
  outputs.lib.mkModule config ["services" "pinchflat"] {
    sops = {
      secrets = {
        pinchflat_password = {};
      };
      templates."pinchflat-env".content = ''
        BASIC_AUTH_PASSWORD=${config.sops.placeholder.pinchflat_password}
      '';
    };

    nixos = {
      system.userActivationScripts."slskd".text = ''
        mkdir -p "${data}/data" \
                 "${media}/downloads/music"
      '';

      virtualisation.oci-containers.containers = {
        "pinchflat" = outputs.lib.mkContainer config {
          image = "ghcr.io/kieraneglin/pinchflat:latest";
          ports = [
            "8945:8945"
          ];
          volumes = [
            "${data}/data:/config"
            "${media}/downloads/music:/downloads"
          ];
          environment = {
            BASIC_AUTH_USERNAME = name;
            TZ = "Europe/Oslo";
          };
          environmentFiles = [
            config.sops.templates."pinchflat-env".path
          ];
        };
      };

      services.nginx.virtualHosts = {
        "ytdl.${config.modules.nginx.domain}" = {
          forceSSL = true;
          sslCertificate = config.modules.nginx.cert;
          sslCertificateKey = config.modules.nginx.key;
          locations."/" = {
            proxyPass = "http://0.0.0.0:8945";
          };
        };
      };
    };
  }
