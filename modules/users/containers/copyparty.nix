{ config, osConfig, outputs, name, ... }:

let
  service = "copyparty";
  self = config.modules.services."${service}";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkServiceModule config "${service}" {
    home.activation."${service}" = ''
      mkdir -p ${base}/config \
               ${media}/music
    '';

  nixos = {
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
      "fs.${self.domain}" = {
        forceSSL = true;
        sslCertificate = self.cert;
        sslCertificateKey = self.key;
        locations."/".proxyPass = "http://0.0.0.0:3923";
      };
    };

    sops.secrets.copyparty_runar = { };
    sops.templates."copyparty-cfg" = {
      owner = name;
      content = ''
        [global]
          e2dsa
          e2ts
          ansi

          df: 2           # stop accepting uploads if less than 2 GB free disk space
          # theme: 2         # monokai
          name: files
          no-robots, force-js

        [accounts]
          ${name}: ${osConfig.sops.placeholder.copyparty_runar}

        [/]
          /w
          accs:
            r: *
            rwmda: ${name}
      '';
    };

  };
}
