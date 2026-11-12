{ self, lib', ... }:
{
  flake.nixosModules.copyparty = lib'.mkService {
    inherit self;

    name = "copyparty";
    subdomain = "fs";
    port = 3923;
    websockets = true;
    maxBodySize = "5G";

    module =
      { config, ... }:
      let
        inherit (config.features.containers) dirs user account;
      in
      {
        sops = {
          secrets.copyparty_password = { };

          templates."copyparty.conf" = {
            owner = user;
            content = ''
              [global]
                e2dsa
                e2ts
                ansi

                df: 2            # stop accepting uploads below 2 GB free
                theme: 2         # monokai
                name: files
                no-robots, force-js

                xff-hdr: cf-connecting-ip
                xff-src: lan
                rproxy: 1

              [accounts]
                ${account}: ${config.sops.placeholder.copyparty_password}

              [/]
                /media
                accs:
                  rwmda: ${account}

              [/music]
                /media/music
                accs:
                  g: *    # everyone can access files if they know the url
                  rwmda: ${account}

              [/p]
                /public
                accs:
                  g: *
                  rwmda: ${account}
            '';
          };
        };

        features.containers.units.copyparty = {
          image = "ghcr.io/9001/copyparty-ac:latest";
          ports = [ "3923:3923" ];
          volumes = [
            "${dirs.containers}/copyparty/data:/cfg"
            "${dirs.media}:/media"
            "${config.sops.templates."copyparty.conf".path}:/cfg/copyparty.conf:ro"
          ];
        };
      };
  };
}
