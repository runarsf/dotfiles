{ config, outputs, pkgs, name, ... }:

outputs.lib.mkModule config "webdav" {
  modules.nginx.enable = true;

  nixos = {
    # https://nixos.wiki/wiki/Nginx#Authentication_via_PAM
    security.pam.services.nginx.setEnvironment = false;
    systemd.services.nginx.serviceConfig = {
      SupplementaryGroups = [ "shadow" ];
    };

    networking.firewall = {
      allowedTCPPorts = [ 6060 ];
      allowedUDPPorts = [ 6060 ];
    };
    systemd.services.webdav = {
      after = [ "network.target" ];
      description = "rclone webdav server";
      wantedBy = [ "default.target" ];

      serviceConfig = {
        ExecStart = builtins.toString (pkgs.writeShellScript "rclone-dav" ''
          #!/run/current-system/sw/bin/bash
          set -o errexit
          set -o nounset

          ${pkgs.rclone}/bin/rclone serve webdav ${config.home.homeDirectory}/data/media/music --addr 0.0.0.0:6060 --user ${name} --pass penis
        '');
        Restart = "always";
      };
    };

    # services.webdav = {
    #   enable = true;
    #   settings = {
    #     address = "0.0.0.0";
    #     port = 6060;
    #     modify = true;
    #     auth = false;
    #     directory = "${config.home.homeDirectory}/data/media/music";
    #   };
    # };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "dav.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "http://0.0.0.0:6060";
          root = "${config.home.homeDirectory}/data/media/music";
          extraConfig = ''
            dav_methods PUT DELETE MKCOL COPY MOVE;
            dav_ext_methods PROPFIND OPTIONS;
            dav_access user:rw group:rw all:r;

            autoindex on;

            client_max_body_size 0;
            create_full_put_path on;
            client_body_temp_path /tmp/nginx/client-bodies;

            auth_pam "Password Required";
            auth_pam_service_name "nginx";
          '';
        };
      };
    };
  };
}
