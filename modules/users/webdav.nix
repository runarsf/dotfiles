{ config, outputs, ... }:

outputs.lib.mkModule config "webdav" {
  modules.nginx.enable = true;

  nixos = {
    # https://nixos.wiki/wiki/Nginx#Authentication_via_PAM
    security.pam.services.nginx.setEnvironment = false;
    systemd.services.nginx.serviceConfig = {
      SupplementaryGroups = [ "shadow" ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "dav.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          root = "${config.home.homeDirectory}/data/media/music";
          index = "index.html index.htm index.nginx-debian.html";
          extraConfig = ''
            dav_methods PUT DELETE MKCOL COPY MOVE;
            dav_ext_methods PROPFIND OPTIONS;
            dav_access user:rw group:rw all:rw;

            client_max_body_size 0;
            create_full_put_path on;
            client_body_temp_path /tmp/;

            auth_pam  "Password Required";
            auth_pam_service_name "nginx";

            # Deny all access unless authenticated
            satisfy all;
            allow all;  # This allows all authenticated users
            deny all;   # This denies all other users
          '';
        };
      };
    };
  };
}
