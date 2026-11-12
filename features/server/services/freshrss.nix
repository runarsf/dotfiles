{ self, lib', ... }:
{
  flake.nixosModules.freshrss = lib'.mkService {
    inherit self;

    name = "freshrss";
    subdomain = "rss";
    port = 4655;

    module =
      { config, ... }:
      let
        inherit (config.features.containers) dirs;
      in
      {
        features.containers.units.freshrss = {
          image = "docker.io/freshrss/freshrss:latest";
          ports = [ "4655:80" ];
          volumes = [
            "${dirs.containers}/freshrss/data:/var/www/FreshRSS/data"
            "${dirs.containers}/freshrss/extensions:/var/www/FreshRSS/extensions"
          ];
          environment = {
            TZ = "Europe/Oslo";
            CRON_MIN = "2,32";
          };
          # The image's entrypoint runs as root inside the container.
          userNS = "keep-id:uid=0,gid=0";
        };
      };
  };
}
