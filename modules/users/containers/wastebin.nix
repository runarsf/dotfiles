{
  config,
  outputs,
  ...
}:
outputs.lib.mkModule config ["containers" "wastebin"] {
  services.podman.containers = {
    "wastebin" = outputs.lib.mkContainer config {
      image = "docker.io/quxfoo/wastebin:latest";
      ports = [
        "8088:8088"
      ];
      volumes = with config.modules.containers.dirs; [
        "${containers}/wastebin/data:/data"
      ];
      environment = {
        WASTEBIN_DATABASE_PATH = "/data/state.db";
      };
    };
  };

  nixos = {
    system.userActivationScripts."wastebin".text = ''
      setfacl -m u:10001:rwx "${config.modules.containers.dirs.containers}/wastebin/data"
    '';
    services.nginx.virtualHosts = {
      "p.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8088";
        };
        extraConfig = ''
          limit_req zone=mylimit burst=20 nodelay;
          client_max_body_size 5G;
        '';
      };
    };
  };
}
