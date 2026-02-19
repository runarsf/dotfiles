{
  outputs,
  config,
  ...
}:
outputs.lib.mkModule config "teleport" {
  nixos = {
    services.teleport = {
      enable = true;
      settings = {
        proxy_service = {
          enabled = true;
          web_listen_addr = "0.0.0.0:3080"; # Web UI and API port
          public_addr = "tp.${config.modules.nginx.domain}"; # Public address clients use
          # ssh_public_addr = "teleport.${config.modules.nginx.domain}:3023"; # SSH proxy port
          tunnel_listen_addr = "0.0.0.0:3024"; # Reverse tunnel port
        };
        auth_service = {
          enabled = true;
          listen_addr = "0.0.0.0:3025";
        };
        ssh_service = {
          enabled = true;
          listen_addr = "0.0.0.0:3022";
        };
      };
    };

    services.nginx.virtualHosts = {
      "tp.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "https://0.0.0.0:3080";
          proxyWebsockets = true;
        };
      };
    };
  };
}
