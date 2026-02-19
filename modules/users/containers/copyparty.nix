{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config ["containers" "copyparty"] {
  sops = {
    secrets = {
      copyparty_password = {};
    };
    templates."copyparty.conf".content = ''
      [global]
        e2dsa
        e2ts
        ansi

        df: 2           # stop accepting uploads if less than 2 GB free disk space
        theme: 2         # monokai
        name: files
        no-robots, force-js

        xff-hdr: cf-connecting-ip
        xff-src: lan
        rproxy: 1

      [accounts]
        ${config.modules.containers.username}: ${config.sops.placeholder.copyparty_password}

      [/]
        /media
        accs:
          rwmda: ${config.modules.containers.username}

      [/music]
        /media/music
        accs:
          g: *    # everyone can access files if they know the url
          rwmda: ${config.modules.containers.username}
    '';
  };

  services.podman.containers = {
    "copyparty" = outputs.lib.mkContainer config {
      image = "ghcr.io/9001/copyparty-ac:latest";
      ports = [
        "3923:3923" # http
      ];
      volumes = with config.modules.containers.dirs; [
        "${containers}/copyparty/data:/cfg"
        "${media}:/media"
        "${config.sops.templates."copyparty.conf".path}:/cfg/copyparty.conf:ro"
      ];
    };
  };

  nixos = {
    services.nginx.virtualHosts = {
      "fs.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "http://127.0.0.1:3923";
          proxyWebsockets = true;
          extraConfig = ''
            client_max_body_size 5G;
          '';
        };
      };
    };
  };
}
