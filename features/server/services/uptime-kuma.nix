{ self, lib', ... }:
{
  flake.nixosModules.uptimeKuma = lib'.mkService {
    inherit self;

    name = "uptimeKuma";
    subdomain = "uptime";
    port = 3001;
    websockets = true;

    module =
      { config, ... }:
      let
        inherit (config.features.containers) dirs user;
        uid = toString config.users.users.${user}.uid;
      in
      {
        features.containers.units.uptime-kuma = {
          image = "ghcr.io/louislam/uptime-kuma:2";
          ports = [ "3001:3001" ];
          volumes = [
            "${dirs.containers}/uptime-kuma/data:/app/data"
            # For container monitoring; needs that user's podman.socket active.
            "/run/user/${uid}/podman/podman.sock:/podman.sock:ro"
          ];
        };
      };
  };
}
