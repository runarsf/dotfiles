{
  config,
  inputs,
  outputs,
  system,
  hostname,
  name,
  pkgs,
  ...
}:
outputs.lib.mkFor system hostname {
  common = {
    imports =
      outputs.lib.concatPaths {paths = ../../modules/users;};

    modules = {
      sops = {
        enable = true;
        ageKeys = ["${config.home.homeDirectory}/.ssh/id_nix"];
      };
    };
  };

  systems = {
    linux = {
      nixos = {
        users.users."${name}" = {
          isSystemUser = true;
          uid = 10001;
          home = "/var/lib/${name}";
          createHome = true;
          shell = pkgs.bash;
          group = name;
        };
        users.groups."${name}" = {
          inherit name;
          gid = 10001;
        };
      };
    };
  };

  hosts = {
    anuc = {
      isDesktop = false;

      modules =
        outputs.lib.enable [
          "tmux"
          "teleport"
          "podman"
          # ["services" "immich"]
          # ["services" "jellyfin"]
          # ["services" "slskd"]
          # ["services" "pinchflat"]
          # ["services" "wastebin"]
          # ["services" "wrtag"]
        ]
        // {
          containers = {
            enable = true;
            username = "runar";
            slskd.enable = true;
            wrtag.enable = true;
            jellyfin.enable = true;
            copyparty.enable = true;
            wastebin.enable = true;
            freshrss.enable = true;
            solidtime.enable = true;
            mealie.enable = true;
            stremio.enable = true;
          };
          nginx = {
            enable = true;
            domains = ["runar.ch"];
            email = "ssl@runar.ch";
          };
        };
    };
  };
}
