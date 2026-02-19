{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config ["containers" "jellyfin"] {
  services.podman.containers = {
    "jellyfin" = outputs.lib.mkContainer config {
      image = "docker.io/jellyfin/jellyfin:latest";
      # user = config.nixos.users.users."${name}".uid;
      # group = config.nixos.users.groups."${name}".gid;
      ports = [
        "8096:8096" # http
        # "8920:8920" # https
        "7359:7359/udp" # client discovery
        "1900:1900/udp" # DLNA service discovery
      ];
      volumes = with config.modules.containers.dirs; [
        "${containers}/jellyfin/data:/config"
        "jellyfin-cache:/cache:Z"
        "${media}/series:/data/tvshows"
        "${media}/movies:/data/movies"
        "${media}/music:/data/music"
      ];
      environment = {
        # PUID = config.nixos.users.users."${name}".uid;
        # PGID = config.nixos.users.groups."${name}".gid;
        # UMASK = "002";

        TZ = "Europe/Oslo";
        JELLYFIN_PublishedServerUrl = "https://jellyfin.${config.modules.nginx.domain}";
      };
    };
  };

  nixos = {
    networking.firewall.allowedUDPPorts = [
      7359
      1900
    ];

    services.nginx.virtualHosts = {
      "jellyfin.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        extraConfig = ''
          client_max_body_size 2G;
        '';
        locations."/" = {
          proxyPass = "http://0.0.0.0:8096";
          proxyWebsockets = true;
        };
      };
    };
  };
}
