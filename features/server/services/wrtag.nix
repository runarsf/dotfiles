{ self, lib', ... }:
{
  flake.nixosModules.wrtag = lib'.mkService {
    inherit self;

    name = "wrtag";
    port = 4545;

    module =
      { config, ... }:
      let
        cfg = config.features.wrtag;
        inherit (config.features.containers) dirs user;
      in
      {
        sops = {
          secrets = {
            wrtag_web_api_key = { };
            telegram_token = { };
            telegram_chat_id = { };
          };

          templates."wrtag-env" = {
            owner = user;
            content = ''
              WRTAG_WEB_API_KEY=${config.sops.placeholder.wrtag_web_api_key}
            '';
          };

          templates."wrtag-config" = {
            owner = user;
            content = ''
              path-format /music/{{ artists .Release.Artists | sort | join "; " | safepathUnicode }}/({{ .Release.ReleaseGroup.FirstReleaseDate.Year }}) {{ .Release.Title | safepathUnicode }}{{ if not (eq .ReleaseDisambiguation "") }} ({{ .ReleaseDisambiguation | safepathUnicode }}){{ end }}/{{ pad0 2 .Track.Position }}.{{ .Media.TrackCount | pad0 2 }} {{ if .IsCompilation }}{{ artistsString .Track.Artists | safepathUnicode }} - {{ end }}{{ .Track.Title | safepathUnicode }}{{ .Ext }}

              addon lyrics lrclib musixmatch genius
              addon replaygain
              addon musicdesc

              notification-uri complete,needs-input telegram://${config.sops.placeholder.telegram_token}@telegram?chats=${config.sops.placeholder.telegram_chat_id}
            '';
          };
        };

        features.containers.units.wrtag = {
          image = "ghcr.io/sentriz/wrtag:latest";
          ports = [ "4545:8080" ];
          volumes = [
            "${dirs.containers}/wrtag/data:/data"
            "${dirs.media}/music:/music"
            "${dirs.media}/downloads/music:/downloads"
            "${config.sops.templates."wrtag-config".path}:/config:ro"
          ];
          environment = {
            WRTAG_WEB_LISTEN_ADDR = ":8080";
            WRTAG_WEB_PUBLIC_URL = "https://${cfg.domain}";
            WRTAG_WEB_DB_PATH = "/data/wrtag.db";
            WRTAG_CONFIG_PATH = "/config";
          };
          environmentFiles = [ config.sops.templates."wrtag-env".path ];
        };
      };
  };
}
