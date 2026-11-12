{ self, lib', ... }:
{
  flake.nixosModules.karakeep = lib'.mkService {
    inherit self;

    name = "karakeep";
    subdomain = "links";
    port = 3000;

    module =
      { config, lib, ... }:
      let
        cfg = config.features.karakeep;
        inherit (config.features.containers) dirs user;
      in
      {
        sops = {
          secrets = {
            karakeep_meili_master_key = { };
            karakeep_nextauth_secret = { };
          };

          templates."karakeep-env" = {
            owner = user;
            content = ''
              MEILI_MASTER_KEY=${config.sops.placeholder.karakeep_meili_master_key}
              NEXTAUTH_SECRET=${config.sops.placeholder.karakeep_nextauth_secret}
            '';
          };

          templates."karakeep-meili-env" = {
            owner = user;
            content = ''
              MEILI_MASTER_KEY=${config.sops.placeholder.karakeep_meili_master_key}
            '';
          };
        };

        features.containers = {
          # karakeep, meili and chrome find each other by unit name here.
          networks.karakeep = { };

          units.karakeep = {
            image = "ghcr.io/karakeep-app/karakeep:release";
            ports = [ "3000:3000" ];
            volumes = [ "${dirs.containers}/karakeep/data:/data" ];
            # https://docs.karakeep.app/configuration/environment-variables/
            environment = {
              MEILI_ADDR = "http://karakeep-meili:7700";
              BROWSER_WEB_URL = "http://karakeep-chrome:9222";
              DATA_DIR = "/data";
              NEXTAUTH_URL = "https://${cfg.domain}";
              DISABLE_SIGNUPS = "true";

              CRAWLER_DOWNLOAD_BANNER_IMAGE = "true";
              CRAWLER_STORE_SCREENSHOT = "true";
              CRAWLER_STORE_PDF = "true";
              CRAWLER_FULL_PAGE_ARCHIVE = "false";
            };
            environmentFiles = [ config.sops.templates."karakeep-env".path ];
            network = "karakeep";
          };

          units.karakeep-chrome = {
            image = "gcr.io/zenika-hub/alpine-chrome:124";
            network = "karakeep";
            exec = lib.concatStringsSep " " [
              "--no-sandbox"
              "--disable-gpu"
              "--disable-dev-shm-usage"
              "--remote-debugging-address=0.0.0.0"
              "--remote-debugging-port=9222"
              "--hide-scrollbars"
              "--disable-blink-features=AutomationControlled"
              "--window-size=1440,900"
            ];
          };

          units.karakeep-meili = {
            image = "docker.io/getmeili/meilisearch:v1.41.0";
            volumes = [ "${dirs.containers}/karakeep/meili:/meili_data" ];
            environment = {
              MEILI_NO_ANALYTICS = "true";
            };
            environmentFiles = [ config.sops.templates."karakeep-meili-env".path ];
            network = "karakeep";
          };
        };
      };
  };
}
