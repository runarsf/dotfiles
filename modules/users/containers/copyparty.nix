{ config, osConfig, outputs, name, ... }:

let
  service = "copyparty";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkModule config "${service}" {
  nixos = {
    system.userActivationScripts."${service}".text = ''
      mkdir -p ${base}/config \
               ${media}/music
    '';

    virtualisation.oci-containers.containers."${service}" = {
      image = "ghcr.io/9001/copyparty-ac:latest";
      extraOptions =
        [ "--pull=newer" ];
      ports = [ "3923:3923" ];
      volumes = [
        "${base}/config:/cfg"
        "${media}/music:/w"
        "${osConfig.sops.templates."copyparty-cfg".path}:/cfg/copyparty.conf:ro"
      ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "files.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/".proxyPass = "http://0.0.0.0:3923";
      };
    };

    sops.secrets.copyparty_runar = { };
    sops.templates."copyparty-cfg" = {
      owner = name;
      content = ''
        [global]
          e2dsa  # enable file indexing and filesystem scanning
          e2ts   # enable multimedia indexing
          ansi   # enable colors in log messages

          # q, lo: /cfg/log/%Y-%m%d.log   # log to file instead of docker

          # p: 3939          # listen on another port
          # ipa: 10.89.      # only allow connections from 10.89.*
          df: 2           # stop accepting uploads if less than 2 GB free disk space
          # ver              # show copyparty version in the controlpanel
          # grid             # show thumbnails/grid-view by default
          # theme: 2         # monokai
          name: files  # change the server-name that's displayed in the browser
          # stats, nos-dup   # enable the prometheus endpoint, but disable the dupes counter (too slow)
          no-robots, force-js  # make it harder for search engines to read your server

        [accounts]
          ${name}: ${osConfig.sops.placeholder.copyparty_runar}   # username: password

        [/]            # create a volume at "/" (the webroot), which will
          /w           # share /w (the docker data volume)
          accs:
            # rw: *      # everyone gets read-write access, but
            rwmda: ${name} # the user "ed" gets read-write-move-delete-admin
      '';
    };

  };
}

