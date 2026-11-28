{ self, lib', ... }:
{
  flake.nixosModules.terminus = lib'.mkService {
    inherit self;

    name = "terminus";
    subdomain = "trmnl";
    port = 2300;
    # Screen and image uploads go through nginx.
    maxBodySize = "25m";

    module =
      { config, ... }:
      let
        cfg = config.modules.terminus;
        inherit (config.modules.containers) dirs user;

        image = "ghcr.io/usetrmnl/terminus:latest";

        # web and worker share fonts (mounted twice, as in upstream's compose)
        # and uploads.
        volumes = [
          "${dirs.containers}/terminus/fonts:/app/public/fonts"
          "${dirs.containers}/terminus/fonts:/usr/share/fonts/terminus"
          "${dirs.containers}/terminus/uploads:/app/public/uploads"
        ];

        # https://github.com/usetrmnl/terminus/blob/main/doc/configuration.adoc
        environment = {
          HANAMI_PORT = "2300";
          # Must match the server URL the device is set up with.
          API_URI = "https://${cfg.domain}";
        };
      in
      {
        sops = {
          secrets = {
            # At least 64 characters: openssl rand -hex 32
            "terminus/app_secret" = { };
            # These go into URLs, so stick to hex: openssl rand -hex 24
            "terminus/db_password" = { };
            "terminus/valkey_password" = { };
          };

          templates."terminus-env" = {
            owner = user;
            content = ''
              APP_SECRET=${config.sops.placeholder."terminus/app_secret"}
              DATABASE_URL=postgres://terminus:${
                config.sops.placeholder."terminus/db_password"
              }@terminus-db:5432/terminus
              KEYVALUE_URL=redis://:${config.sops.placeholder."terminus/valkey_password"}@terminus-valkey:6379/0
            '';
          };

          templates."terminus-db-env" = {
            owner = user;
            content = ''
              POSTGRES_PASSWORD=${config.sops.placeholder."terminus/db_password"}
            '';
          };

          # A config file rather than --requirepass, so the password stays out
          # of the world-readable unit.
          templates."terminus-valkey-conf" = {
            owner = user;
            content = ''
              requirepass ${config.sops.placeholder."terminus/valkey_password"}
              maxmemory 512mb
              maxmemory-policy noeviction
              dir /data
            '';
          };
        };

        modules.containers = {
          # web, worker, db and valkey find each other by unit name here.
          networks.terminus = { };

          units.terminus = {
            inherit image volumes;
            ports = [ "2300:2300" ];
            environment = environment // {
              # Compiles assets and runs migrations before starting puma.
              APP_SETUP = "true";
            };
            environmentFiles = [ config.sops.templates."terminus-env".path ];
            network = "terminus";
            # The image runs as uid 1000 and owns /app/tmp and
            # /app/public/assets as that user.
            userNS = "keep-id:uid=1000,gid=1000";
            # The entrypoint runs migrations without checking whether they
            # worked, so wait for postgres before it starts rather than after.
            podmanArgs = [
              "--init"
              "--entrypoint=/bin/bash"
            ];
            exec = "-c 'until pg_isready -q -h terminus-db -U terminus; do sleep 2; done; exec scripts/docker/entrypoint bundle exec puma --config ./config/puma.rb'";
          };

          units.terminus-worker = {
            inherit image volumes environment;
            environmentFiles = [ config.sops.templates."terminus-env".path ];
            network = "terminus";
            userNS = "keep-id:uid=1000,gid=1000";
            podmanArgs = [ "--init" ];
            # Upstream starts the worker once web is healthy, so after migrations.
            exec = "bash -c 'until curl -fs http://terminus:2300/up >/dev/null; do sleep 5; done; exec bundle exec sidekiq -r ./config/sidekiq.rb'";
          };

          units.terminus-db = {
            # Major upgrades need a dump and restore, so bump this by hand.
            image = "docker.io/library/postgres:19.0-alpine";
            volumes = [ "${dirs.containers}/terminus/postgres:/var/lib/postgresql" ];
            environment = {
              POSTGRES_USER = "terminus";
              POSTGRES_DB = "terminus";
            };
            environmentFiles = [ config.sops.templates."terminus-db-env".path ];
            network = "terminus";
          };

          units.terminus-valkey = {
            image = "docker.io/valkey/valkey:9.2-alpine";
            volumes = [
              "${dirs.containers}/terminus/valkey:/data"
              "${config.sops.templates."terminus-valkey-conf".path}:/etc/valkey/valkey.conf:ro"
            ];
            exec = "valkey-server /etc/valkey/valkey.conf";
            network = "terminus";
          };
        };
      };
  };
}
