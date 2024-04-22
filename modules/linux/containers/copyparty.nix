{ config, domain, cert, key, ... }:

let base = "${config.users.users.ops.home}/data/containers/copyparty";

in {
  # networking.firewall = {
  #   allowedTCPPorts = [ 8384 22000 ];
  #   allowedUDPPorts = [ 8384 22000 21027 ];
  # };

  system.userActivationScripts.copyparty-data.text = ''
    mkdir -p ${base}/config \
             /home/runar/data/music
  '';

  virtualisation.oci-containers.containers.copyparty = {
    image = "ghcr.io/9001/copyparty-dj:latest";
    extraOptions = [ "--pull=newer" ];
    ports = [ "3923:3923" ];
    volumes = [
      "${config.users.users.ops.home}/data/music:/w"
      "${base}/config:/cfg"
      "${config.sops.templates."copyparty-cfg".path}:/cfg/copyparty.conf:ro"
    ];
  };

  services.nginx.virtualHosts."files.${domain}" = {
    forceSSL = true;
    sslCertificate = cert;
    sslCertificateKey = key;
    locations."/".proxyPass = "http://0.0.0.0:3923";
  };

  # systemd.services.copyparty-cfg = {
  #   wantedBy = [ "podman-copyparty.target" "multi-user.target" ];
  #   after = [ "sops-nix.service" ];
  #   script = ''
  #     mkdir -p ${base}
  #     ln -sf ${config.sops.templates."copyparty-cfg".path} ${base}/config/copyparty.conf
  #   '';
  # };

  sops.secrets.copyparty_runar = {};
  sops.templates."copyparty-cfg" = {
    owner = "runar";
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
        runar: ${config.sops.placeholder.copyparty_runar}   # username: password
      
      [/]            # create a volume at "/" (the webroot), which will
        /w           # share /w (the docker data volume)
        accs:
          # rw: *      # everyone gets read-write access, but
          rwmda: runar  # the user "ed" gets read-write-move-delete-admin
    '';
  };
}

