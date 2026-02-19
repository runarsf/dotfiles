{
  config,
  outputs,
  ...
}: let
  cfg = config.modules.services.immich;
  base = "${config.home.homeDirectory}/data/containers/immich";
  media = "${config.home.homeDirectory}/data/media";
in
  outputs.lib.mkModule config ["services" "immich"] rec {
    sops = {
      secrets.immich_db_password = {};
      templates."immich-env".content = ''
        IMMICH_VERSION=release

        # Please use only the characters `A-Za-z0-9`, without special characters or spaces
        DB_PASSWORD=${config.sops.placeholder.immich_db_password}
        POSTGRES_PASSWORD=${config.sops.placeholder.immich_db_password}

        REDIS_HOSTNAME=${nixos.virtualisation.oci-containers.containers.immich_redis.hostname}
        DB_HOSTNAME=${nixos.virtualisation.oci-containers.containers.immich_postgres.hostname}
        DB_USERNAME=postgres
        DB_DATABASE_NAME=immich
      '';
    };

    nixos = {
      system.userActivationScripts."immich".text = ''
        mkdir -p "${base}/db" \
                 "${media}/photos"
      '';

      virtualisation.oci-containers.containers = {
        "immich_server" = {
          image = "ghcr.io/immich-app/immich-server:release";
          hostname = "immich_server";
          extraOptions = ["--pull=newer"];
          ports = ["2283:2283"];
          volumes = [
            "${media}/photos:/data"
            "/etc/localtime:/etc/localtime:ro"
          ];
          environmentFiles = [
            config.sops.templates."immich-env".path
          ];
          dependsOn = [
            "immich_redis"
            "immich_postgres"
          ];
        };

        "immich_machine_learning" = {
          image = "ghcr.io/immich-app/immich-machine-learning:release";
          hostname = "immich_machine_learning";
          extraOptions = ["--pull=newer"];
          volumes = [
            "model-cache:/cache"
          ];
          environmentFiles = [
            config.sops.templates."immich-env".path
          ];
        };

        "immich_redis" = {
          image = "docker.io/valkey/valkey:8-bookworm@sha256:fea8b3e67b15729d4bb70589eb03367bab9ad1ee89c876f54327fc7c6e618571";
          hostname = "immich_redis";
        };

        "immich_postgres" = {
          image = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:bcf63357191b76a916ae5eb93464d65c07511da41e3bf7a8416db519b40b1c23";
          hostname = "immich_postgres";
          volumes = [
            "${base}/db:/var/lib/postgresql/data"
          ];
          environment = {
            POSTGRES_USER = "postgres";
            POSTGRES_DB = "immich";
            POSTGRES_INITDB_ARGS = "--data-checksums";
          };
          environmentFiles = [
            config.sops.templates."immich-env".path
          ];
        };
      };

      services.nginx.virtualHosts = {
        "photos.${config.modules.nginx.domain}" = {
          forceSSL = true;
          sslCertificate = config.modules.nginx.cert;
          sslCertificateKey = config.modules.nginx.key;
          locations."/" = {
            proxyPass = "http://0.0.0.0:2283";
            proxyWebsockets = true;
          };
        };
      };
    };
  }
