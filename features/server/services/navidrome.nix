{ self, lib', ... }:
{
  flake.nixosModules.navidrome = lib'.mkService {
    inherit self;

    name = "navidrome";
    port = 4533;
    websockets = true;
    maxBodySize = "2G";

    module =
      { config, ... }:
      let
        inherit (config.features.containers) dirs;
      in
      {
        features.containers.units.navidrome = {
          image = "ghcr.io/navidrome/navidrome:latest";
          ports = [ "4533:4533" ];
          volumes = [
            "${dirs.containers}/navidrome/data:/data"
            "${dirs.media}/music:/music:ro"
          ];
          environment = {
            ND_ENABLESHARING = "true";
          };
        };
      };
  };
}
