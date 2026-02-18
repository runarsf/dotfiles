{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config ["containers" "solidtime"] {
  sops = {
    secrets = {
      solidtime_db_password = {};
    };
    templates."solidtime-db-env".content = ''
      PGPASSWORD=${config.sops.placeholder.solidtime_db_password}
      POSTGRES_DB=solidtime
      POSTGRES_USER=solidtime
      POSTGRES_PASSWORD=${config.sops.placeholder.solidtime_db_password}
    '';
    templates."solidtime-env".content = ''
      APP_DOMAIN=time.${config.modules.nginx.domain}
      DB_DATABASE=solidtime
      DB_USERNAME=solidtime
      FORWARD_APP_PORT=8000
      FORWARD_DB_PORT=5432
      DB_PASSWORD=${config.sops.placeholder.solidtime_db_password}
      SOLIDTIME_IMAGE_TAG=latest

      # Laravel env
      APP_NAME="solidtime"
      VITE_APP_NAME="solidtime"
      APP_ENV="production"
      APP_DEBUG="false"
      APP_URL="https://time.${config.modules.nginx.domain}"
      APP_FORCE_HTTPS="true"
      TRUSTED_PROXIES="0.0.0.0/0,2000:0:0:0:0:0:0:0/3"
      APP_ENABLE_REGISTRATION="false"

      # Authentication https://docs.solidtime.io/self-hosting/configuration
      # php artisan self-host:generate-keys
      APP_KEY="${config.sops.placeholder.solidtime_app_key}"
      PASSPORT_PRIVATE_KEY="${config.sops.placeholder.solidtime_private_key}"
      PASSPORT_PUBLIC_KEY="${config.sops.placeholder.solidtime_public_key}"

      SUPER_ADMINS="${config.sops.placeholder.solidtime_super_admins}"

      # Logging
      LOG_CHANNEL="stderr_daily"
      LOG_LEVEL="debug"

      # Database
      DB_CONNECTION="pgsql"
      DB_HOST="solidtime-db"
      DB_PORT="5432"
      DB_SSLMODE="require"
      DB_DATABASE="solidtime"
      DB_USERNAME="solidtime"
      DB_PASSWORD="${config.sops.placeholder.solidtime_db_password}"

      # Mail
      MAIL_MAILER="smtp"
      MAIL_HOST="smtp.gmail.com"
      MAIL_PORT="587"
      MAIL_ENCRYPTION="tls"
      MAIL_FROM_ADDRESS="${config.sops.placeholder.solidtime_mail_user}"
      MAIL_FROM_NAME="Solidtime"
      MAIL_USERNAME="${config.sops.placeholder.solidtime_mail_user}"
      MAIL_PASSWORD="${config.sops.placeholder.solidtime_mail_password}"

      # Queue
      QUEUE_CONNECTION="database"

      # File storage
      FILESYSTEM_DISK="local"
      PUBLIC_FILESYSTEM_DISK="public"

      # Services
      GOTENBERG_URL="http://172.0.0.1:7130"
    '';
  };

  services.podman.networks."solidtime" = {
    driver = "bridge";
  };
  services.podman.containers = let
    base = {
      image = "docker.io/solidtime/solidtime:latest";
      volumes = with config.modules.containers.dirs; [
        "${containers}/solidtime/data:/var/www/html/storage"
        "${containers}/solidtime/cache:/var/www/html/storage/framework/cache"
        "${containers}/solidtime/views:/var/www/html/storage/framework/views"
        "${containers}/solidtime/logs:/var/www/html/storage/logs"
        "${containers}/solidtime/app:/var/www/html/storage/app"
      ];
      environmentFile = [
        "${config.sops.templates."solidtime-env".path}"
      ];
      userNS = "keep-id:uid=1000,gid=1000";
      network = "solidtime";
    };
  in {
    "solidtime" = outputs.lib.mkContainer config (base
      // {
        environment = {
          CONTAINER_MODE = "http";
        };
        ports = [
          "7133:8000" # http
        ];
      });
    "solidtime-scheduler" = outputs.lib.mkContainer config (base
      // {
        environment = {
          CONTAINER_MODE = "scheduler";
          AUTO_DB_MIGRATE = "true";
        };
      });
    "solidtime-queue" = outputs.lib.mkContainer config (base
      // {
        environment = {
          CONTAINER_MODE = "worker";
          WORKER_COMMAND = "php /var/www/html/artisan queue:work";
        };
      });
    "solidtime-db" = outputs.lib.mkContainer config {
      image = "docker.io/postgres:15";
      ports = [
        "7132:5432" # postgres
      ];
      volumes = with config.modules.containers.dirs; [
        "${containers}/solidtime/db:/var/lib/postgresql/data"
      ];
      environmentFile = [
        config.sops.templates."solidtime-db-env".path
      ];
      network = "solidtime";
      # userNS = outputs.lib.mkForce null;
      # extraPodmanArgs = [
      # "--pod=flare-pod"
      # ];
      # extraConfig = {
      # Unit = {
      # After = "podman-create-flare-pod.service";
      # Requires = "podman-create-flare-pod.service";
      # };
      # };
    };
    "solidtime-gotenberg" = outputs.lib.mkContainer config {
      image = "docker.io/gotenberg/gotenberg:8";
      ports = [
        "7130:3000" # gotenberg
      ];
      network = "solidtime";
    };
  };

  nixos = {
    services.nginx.virtualHosts = {
      "time.${config.modules.nginx.domain}" = {
        forceSSL = true;
        sslCertificate = config.modules.nginx.cert;
        sslCertificateKey = config.modules.nginx.key;
        locations."/" = {
          proxyPass = "http://127.0.0.1:7133";
          proxyWebsockets = true;
          extraConfig = ''
            client_max_body_size 5G;
          '';
        };
      };
    };
  };
}
