{ self, lib', ... }:
{
  flake.nixosModules.metadataRemote = lib'.mkService {
    inherit self;

    name = "metadataRemote";
    subdomain = "metadata";
    port = 8338;
    websockets = true;

    module =
      { config, ... }:
      let
        cfg = config.features.metadataRemote;
        inherit (config.features.containers) dirs user account;
        uid = toString config.users.users.${user}.uid;
        gid = toString config.users.groups.${config.users.users.${user}.group}.gid;
      in
      {
        # No authentication of its own, so nginx basic auth. Rendered by sops
        # rather than virtualHosts.<name>.basicAuth, which lands in /nix/store.
        sops = {
          secrets.metadata_remote_password = { };

          templates."metadata-remote-htpasswd" = {
            owner = "nginx";
            content = ''
              ${account}:{PLAIN}${config.sops.placeholder.metadata_remote_password}
            '';
          };
        };

        services.nginx.virtualHosts.${cfg.domain}.basicAuthFile =
          config.sops.templates."metadata-remote-htpasswd".path;

        features.containers.units.metadata-remote = {
          image = "ghcr.io/wow-signal-dev/metadata-remote:latest";
          ports = [ "8338:8338" ];
          volumes = [
            "${dirs.media}/music:/music/library"
            "${dirs.media}/downloads/music:/music/downloads"
          ];
          environment = {
            PUID = uid;
            PGID = gid;
          };
          userNS = "keep-id:uid=0,gid=0";
        };
      };
  };
}
