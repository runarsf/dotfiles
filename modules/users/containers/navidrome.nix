{ config, outputs, ... }:

let
  service = "navidrome";
  base = "${config.home.homeDirectory}/data/containers/${service}";
  media = "${config.home.homeDirectory}/data/media";

in outputs.lib.mkServiceModule config "${service}" {
  nixos = {
    system.userActivationScripts."${service}".text = ''
      mkdir -p ${base}/data \
               ${media}/music
    '';

    virtualisation.oci-containers.containers."${service}" = {
      image = "deluan/navidrome:latest";
      extraOptions =
        [ "--pull=newer" ];
      ports = [ "4533:4533" ];
      environment = {
        ND_SCANSCHEDUL = "1h";
        ND_LOGLEVEL = "info";
        ND_SESSIONTIMEOUT = "72h";
        ND_BASEURL = "";
        ND_ENABLESHARING = "true";
        ND_DEFAULTTHEME = "Spotify-ish";
        ND_ENABLEGRAVATAR = "true";
        ND_JUKEBOX_ENABLED = "true";
      };
      volumes = [
        "${base}/data:/data"
        "${media}/music:/music:ro"
      ];
    };

    services.nginx.virtualHosts = outputs.lib.mkIf config.modules.nginx.enable {
      "fm.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/".proxyPass = "http://0.0.0.0:4533";
      };
    };
  };
}

