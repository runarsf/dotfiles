{ self, lib', ... }:
{
  flake.nixosModules.grimmory = lib'.mkService {
    inherit self;

    name = "grimmory";
    subdomain = "books";
    port = 6060;
    maxBodySize = "2G";

    module =
      { config, ... }:
      let
        inherit (config.features.containers) dirs user;
      in
      {
        sops = {
          secrets.grimmory_db_password = { };

          templates."grimmory-env" = {
            owner = user;
            content = ''
              DATABASE_PASSWORD=${config.sops.placeholder.grimmory_db_password}
            '';
          };

          templates."grimmory-db-env" = {
            owner = user;
            content = ''
              MARIADB_ROOT_PASSWORD=${config.sops.placeholder.grimmory_db_password}
              MARIADB_PASSWORD=${config.sops.placeholder.grimmory_db_password}
            '';
          };
        };

        features.containers = {
          # Reaches its database by unit name over this network.
          networks.grimmory = { };

          units.grimmory = {
            image = "ghcr.io/grimmory-tools/grimmory:latest";
            ports = [ "6060:6060" ];
            volumes = [
              "${dirs.containers}/grimmory/data:/app/data"
              "${dirs.containers}/grimmory/bookdrop:/bookdrop"
              "${dirs.media}/books:/books"
            ];
            environment = {
              USER_ID = "0";
              GROUP_ID = "0";
              TZ = "Europe/Oslo";
              DATABASE_URL = "jdbc:mariadb://grimmory-db:3306/grimmory";
              DATABASE_USERNAME = "grimmory";
              BOOKLORE_PORT = "6060";
            };
            environmentFiles = [ config.sops.templates."grimmory-env".path ];
            network = "grimmory";
            userNS = "keep-id:uid=0,gid=0";
          };

          units.grimmory-db = {
            image = "ghcr.io/mariadb/mariadb:11.4.12-ubi9";
            volumes = [ "${dirs.containers}/grimmory/db:/var/lib/mysql:Z" ];
            environment = {
              MARIADB_USER = "grimmory";
              MARIADB_DATABASE = "grimmory";
            };
            environmentFiles = [ config.sops.templates."grimmory-db-env".path ];
            network = "grimmory";
            # The image runs mysqld as its own mysql user inside.
            userNS = "keep-id:uid=999,gid=999";
          };
        };
      };
  };
}
