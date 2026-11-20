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
        inherit (config.modules.containers) dirs user;
        uid = toString config.users.users.${user}.uid;
      in
      {
        modules.containers.units.uptime-kuma = {
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
