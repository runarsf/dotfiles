{ config, domain, cert, key, ... }:

# FIXME Service account path
let base = "${config.users.users.ops.home}/containers/gonic";

in {
  # networking.firewall = {
  #   allowedTCPPorts = [ 8384 22000 ];
  #   allowedUDPPorts = [ 8384 22000 21027 ];
  # };

  system.userActivationScripts.gonic-data.text = ''
    mkdir -p ${base}/data \
             ${base}/podcasts \
             ${base}/playlists \
             ${base}/cache \
             ${config.users.users.ops.home}/data/music
  '';

  virtualisation.oci-containers.containers.gonic = {
    image = "sentriz/gonic:latest";
    extraOptions = [ "--pull=newer" "--group-add=audio" "--device=/dev/snd:/dev/snd" ];
    ports = [ "4747:80" ];
    environment = {
      TZ = "${config.time.timeZone}";
      GONIC_JUKEBOX_ENABLED = "true";
    };
    volumes = [
      "${base}/data:/data"
      "${config.users.users.ops.home}/data/music:/music:ro"
      "${base}/podcasts:/podcasts"
      "${base}/playlists:/playlists"
      "${base}/cache:/cache"
    ];
  };

  services.nginx.virtualHosts."fm.${domain}" = {
    forceSSL = true;
    sslCertificate = cert;
    sslCertificateKey = key;
    locations."/".proxyPass = "http://0.0.0.0:4747";
  };
}

