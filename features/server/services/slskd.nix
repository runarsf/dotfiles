{ self, lib', ... }:
{
  flake.nixosModules.slskd = lib'.mkService {
    inherit self;

    name = "slskd";
    port = 5030;
    websockets = true;

    module =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        inherit (config.features.containers) dirs user account;

        # Only hand downloads off to wrtag if it is enabled too.
        withWrtag = config.features.wrtag.enable or false;

        # Runs in the container, so wget and jq come from its image. The empty
        # user in the URL passes the API key as a password.
        autotag = pkgs.writeShellScriptBin "autotag.sh" ''
          download_path="$(echo "$SLSKD_SCRIPT_DATA" | jq -r .localDirectoryName)"
          echo "$SLSKD_SCRIPT_DATA" > /app/event

          wget -q -O/dev/null \
               --post-data "path=$download_path" \
               "https://''':$WRTAG_WEB_API_KEY@${config.features.wrtag.domain}/op/move"
        '';
      in
      {
        sops = {
          secrets = {
            slskd_password = { };
            slskd_slsk_username = { };
            slskd_slsk_password = { };
          };

          # Separate from slskd.yml: if slskd rewrites that back to defaults,
          # the web UI still refuses the default login.
          templates."slskd-env" = {
            owner = user;
            content = ''
              SLSKD_PASSWORD=${config.sops.placeholder.slskd_password}
            '';
          };

          templates."slskd.yml" = {
            owner = user;
            content = lib.generators.toYAML { } {
              remoteConfiguration = false;
              web.authentication = {
                disabled = false;
                username = account;
                password = config.sops.placeholder.slskd_password;
              };
              soulseek = {
                username = config.sops.placeholder.slskd_slsk_username;
                password = config.sops.placeholder.slskd_slsk_password;
              };
              integrations = lib.optionalAttrs withWrtag {
                scripts.write_tags = {
                  on = [ "DownloadDirectoryComplete" ];
                  run.command = "bash /autotag.sh";
                };
              };
            };
          };
        };

        features.containers.units.slskd = {
          image = "ghcr.io/slskd/slskd:latest";
          ports = [
            "5030:5030" # http
            "5031:5031" # https
            "50300:50300" # soulseek
          ];
          volumes = [
            "${dirs.containers}/slskd/data:/app"
            "${config.sops.templates."slskd.yml".path}:/app/slskd.yml:ro"
            "${dirs.media}/downloads/music:/downloads"
          ]
          ++ lib.optional withWrtag "${autotag}/bin/autotag.sh:/autotag.sh:ro";
          environment = {
            SLSKD_UMASK = "002";
            SLSKD_USERNAME = account;
            SLSKD_REMOTE_FILE_MANAGEMENT = "true";
            # slskd and wrtag have to agree on this path
            SLSKD_DOWNLOADS_DIR = "/downloads";
          };
          environmentFiles = [
            config.sops.templates."slskd-env".path
          ]
          ++ lib.optional withWrtag config.sops.templates."wrtag-env".path;
        };
      };
  };
}
