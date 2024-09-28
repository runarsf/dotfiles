{ config, outputs, ... }:

let
  container = "koel";
  # TODO  "/var/lib/containers/${container}";
  base = "${config.home.homeDirectory}/data/containers/${container}";

  fuckDockerHub = container:
    builtins.trace
    "Enabling container ${container}, if it doesn't work, try running podman pull ${container}"
    container;

in outputs.lib.mkModule config "${container}" {
  nixos = {
    # networking.firewall = {
    #   allowedTCPPorts = [ 8384 22000 ];
    #   allowedUDPPorts = [ 8384 22000 21027 ];
    # };

    system.userActivationScripts."${container}".text = ''
      mkdir -p ${base}/music \
               ${base}/covers \
               ${base}/search_index \
               ${base}/db
      chown -R 33:33 ${base}/search_index
      chmod -R 775 ${base}/search_index
    '';

    virtualisation.oci-containers.containers = {
      "${container}" = builtins.trace ''
        sudo podman exec --user www-data -it koel bash
        php artisan koel:init --no-assets'' {
          image = fuckDockerHub "docker.io/phanan/koel:latest";
          extraOptions = [ "--pull=newer" ];
          ports = [ "4747:80" ];
          environment = {
            DB_CONNECTION = "mysql";
            DB_HOST = "${container}db";
            DB_PORT = "3306";
            DB_USERNAME = "koel";
            DB_PASSWORD = "passmord";
            DB_DATABASE = "koel";
          };
          volumes = [
            "${base}/music:/music"
            "${base}/covers:/var/www/html/public/img/covers"
            "${base}/search_index:/var/www/html/storage/search-indexes"
          ];
          # dependsOn = [ "${container}-db" ];
        };
      "${container}db" = {
        image = fuckDockerHub "docker.io/mariadb:10.11";
        extraOptions = [ "--pull=newer" ];
        environment = {
          MYSQL_ROOT_PASSWORD = "passmord";
          MYSQL_DATABASE = "koel";
          MYSQL_USER = "koel";
          MYSQL_PASSWORD = "passmord";
        };
        volumes = [ "${base}/db:/var/lib/mysql" ];
      };
      # "${container}db" = {
      #   image = fuckDockerHub "docker.io/library/postgres:13";
      #   extraOptions = [ "--pull=newer" ];
      #   environment = {
      #     POSTGRES_DB = "koel";
      #     POSTGRES_USER = "koel";
      #     POSTGRES_PASSWORD = "passmord";
      #   };
      #   volumes = [
      #     "${base}/db:/var/lib/postgresql/data"
      #   ];
      # };
    };

    #services.nginx.virtualHosts."fm.${domain}" = {
    #  forceSSL = true;
    #  sslCertificate = cert;
    #  sslCertificateKey = key;
    #  locations."/".proxyPass = "http://0.0.0.0:4747";
    #};
  };
}

