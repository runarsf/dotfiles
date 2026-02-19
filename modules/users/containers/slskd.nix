{
  config,
  outputs,
  pkgs,
  ...
}: let
  autotag = pkgs.writeShellScriptBin "autotag.sh" ''
    download_path="$(echo "$SLSKD_SCRIPT_DATA" | jq -r .localDirectoryName)"
    echo "$SLSKD_SCRIPT_DATA" > /app/event

    wget -q -O/dev/null \
         --post-data "path=$download_path" \
         "https://''':$WRTAG_WEB_API_KEY@wrtag.${config.modules.nginx.domain}/op/move"
  '';
in
  outputs.lib.mkModule config ["containers" "slskd"] {
    sops = {
      secrets = {
        slskd_password = {};
        slskd_slsk_username = {};
        slskd_slsk_password = {};
      };
      # This is a workaround for if slskd.yml resets to default, we don't want the web UI to be available and generate a jwt with the default username/password
      templates."slskd-env".content = ''
        SLSKD_PASSWORD=${config.sops.placeholder.slskd_password}
      '';
      templates."slskd.yml".content = pkgs.lib.generators.toYAML {} {
        remoteConfiguration = false;
        web = {
          authentication = {
            disabled = false;
            username = config.modules.containers.username;
            password = config.sops.placeholder.slskd_password;
          };
        };
        soulseek = {
          username = config.sops.placeholder.slskd_slsk_username;
          password = config.sops.placeholder.slskd_slsk_password;
        };
        integration = outputs.lib.optionalAttrs (config.modules.containers.wrtag.enable) {
          scripts = {
            write_tags = {
              on = ["DownloadDirectoryComplete"];
              run = {
                command = "bash /autotag.sh";
              };
            };
          };
        };
      };
    };

    services.podman.containers = {
      "slskd" = outputs.lib.mkContainer config {
        image = "ghcr.io/slskd/slskd:latest";
        ports = [
          "5030:5030" # http
          "5031:5031" # https
          "50300:50300" # soulseek
        ];
        volumes = with config.modules.containers.dirs;
          [
            "${containers}/slskd/data:/app"
            "${config.sops.templates."slskd.yml".path}:/app/slskd.yml:ro"
            "${media}/downloads/music:/downloads"
          ]
          ++ outputs.lib.optionals (config.modules.containers.wrtag.enable) [
            "${autotag}/bin/autotag.sh:/autotag.sh:ro"
          ];
        environment = {
          SLSKD_UMASK = "002";
          SLSKD_USERNAME = config.modules.containers.username;
          SLSKD_REMOTE_FILE_MANAGEMENT = "true";
          SLSKD_DOWNLOADS_DIR = "/downloads"; # slskd and wrtag need to have the same downloads path
        };
        environmentFile =
          [
            config.sops.templates."slskd-env".path
          ]
          ++ outputs.lib.optionals (config.modules.containers.wrtag.enable) [
            config.sops.templates."wrtag-env".path
          ];
      };
    };

    nixos = {
      services.nginx.virtualHosts = {
        "slskd.${config.modules.nginx.domain}" = {
          forceSSL = true;
          sslCertificate = config.modules.nginx.cert;
          sslCertificateKey = config.modules.nginx.key;
          locations."/" = {
            # proxyPass = "http://0.0.0.0:5030";
            proxyPass = "http://127.0.0.1:5030";
            proxyWebsockets = true;
          };
        };
      };
    };
  }
