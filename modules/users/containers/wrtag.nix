{
  config,
  outputs,
  pkgs,
  ...
}:
  outputs.lib.mkModule config ["containers" "wrtag"] {
    sops = {
      secrets = {
        wrtag_web_api_key = {};
        telegram_token = {};
        telegram_chat_id = {};
      };
      templates."wrtag-env".content = ''
        WRTAG_WEB_API_KEY=${config.sops.placeholder.wrtag_web_api_key}
      '';
      templates."wrtag-config".content = ''
        path-format /music/{{ artists .Release.Artists | sort | join "; " | safepath }}/({{ .Release.ReleaseGroup.FirstReleaseDate.Year }}) {{ .Release.Title | safepath }}{{ if not (eq .ReleaseDisambiguation "") }} ({{ .ReleaseDisambiguation | safepath }}){{ end }}/{{ pad0 2 .TrackNum }}.{{ len .Tracks | pad0 2 }} {{ if .IsCompilation }}{{ artistsString .Track.Artists | safepath }} - {{ end }}{{ .Track.Title | safepath }}{{ .Ext }}

        addon lyrics lrclib musixmatch genius
        addon replaygain
        addon musicdesc

        notification-uri complete,needs-input telegram://${config.sops.placeholder.telegram_token}@telegram?chats=${config.sops.placeholder.telegram_chat_id}
      '';
    };

    services.podman.containers = {
      "wrtag" = outputs.lib.mkContainer config {
        image = "ghcr.io/sentriz/wrtag:latest";
        ports = [
          "4545:8080" # http
        ];
        volumes = with config.modules.containers.dirs; [
          "${containers}/wrtag/data:/data"
          "${media}/music:/music"
          "${media}/downloads/music:/downloads"
          "${config.sops.templates."wrtag-config".path}:/config:ro"
        ];
        environment = {
          WRTAG_WEB_LISTEN_ADDR = ":8080";
          WRTAG_WEB_PUBLIC_URL = "https://wrtag.${config.modules.nginx.domain}";
          WRTAG_WEB_DB_PATH = "/data/wrtag.db";
          WRTAG_CONFIG_PATH = "/config";
        };
        environmentFile = [
          config.sops.templates."wrtag-env".path
        ];
      };
    };

    nixos = {
      services.nginx.virtualHosts = {
        "wrtag.${config.modules.nginx.domain}" = {
          forceSSL = true;
          sslCertificate = config.modules.nginx.cert;
          sslCertificateKey = config.modules.nginx.key;
          locations."/" = {
            proxyPass = "http://127.0.0.1:4545";
          };
        };
      };
    };
  }
