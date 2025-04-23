{
  config,
  outputs,
  ...
}: let
  container = "jellyfin";
  base = "${config.home.homeDirectory}/data/containers/${container}";
in
  outputs.lib.mkServiceModule config "${container}" {
    config = {
      nixos = {
        system.userActivationScripts."${container}".text = ''
          mkdir -p ${base}/config \
                   ${base}/series \
                   ${base}/movies \
                   ${base}/music
        '';

        virtualisation.oci-containers.containers = {
          "${container}" = {
            image = "lscr.io/linuxserver/jellyfin:latest";
            extraOptions = ["--pull=newer"];
            ports = ["8096:8096" "8920:8920" "7359:7359/udp" "1900:1900/udp"];
            environment = {
              PUID = "1000";
              PGID = "1000";
              TZ = "Europe/Oslo";
              JELLYFIN_PublishedServerUrl = "0.0.0.0";
            };
            volumes = [
              "${base}/config:/config"
              "${base}/series:/data/tvshows"
              "${base}/movies:/data/movies"
              "${base}/music:/data/music"
            ];
          };
        };
      };
    };
  }
