{self, ...}: {
  flake.nixosModules.containers = {
    config,
    lib,
    ...
  }: let
    cfg = config.features.containers;

    hostPath = vol: builtins.head (lib.splitString ":" vol);

    pathAncestors = path: let
      parts = lib.filter (p: p != "") (lib.splitString "/" path);
    in
      map (n: "/" + lib.concatStringsSep "/" (lib.take n parts)) (lib.range 1 (lib.length parts));

    defaultDirs = with cfg.dirs; [
      data
      containers
      media
      "${media}/music"
      # "${media}/movies"
      # "${media}/series"
      # "${media}/photos"
      "${media}/downloads"
      "${media}/downloads/music"
      # "${media}/downloads/movies"
      # "${media}/downloads/series"
      # "${media}/downloads/photos"
    ];

    allVolumes = lib.flatten (lib.mapAttrsToList (_: u: u.volumes) cfg.units);

    relevantHostPaths =
      map hostPath
      (lib.filter (v: lib.hasPrefix cfg.dirs.data (hostPath v)) allVolumes);

    allDirs = lib.unique (lib.naturalSort (
      lib.flatten (map pathAncestors (relevantHostPaths ++ defaultDirs))
    ));

    mkQuadlet = unit: let
      section = name: entries:
        "[${name}]\n" + lib.concatMapStrings (e: "${e}\n") entries;
    in
      lib.concatStringsSep "\n" [
        (section "Container" (
          ["Image=${unit.image}"]
          ++ map (p: "PublishPort=${p}") unit.ports
          ++ map (v: "Volume=${v}") unit.volumes
          ++ lib.mapAttrsToList (k: v: "Environment=${k}=${v}") unit.environment
        ))
        (section "Service" ["Restart=${unit.restart}"])
        (section "Install" ["WantedBy=default.target"])
      ];
  in {
    imports = [self.nixosModules.podman];

    options.features.containers = {
      user = lib.mkOption {
        type = lib.types.str;
        default = throw "Set features.containers.user to the service user that runs containers";
        description = "System user that owns and runs the containers.";
      };
      dirs = {
        data = lib.mkOption {
          type = lib.types.str;
          default = "/data";
        };
        containers = lib.mkOption {
          type = lib.types.str;
          default = "${cfg.dirs.data}/containers";
        };
        media = lib.mkOption {
          type = lib.types.str;
          default = "${cfg.dirs.data}/media";
        };
      };
      units = lib.mkOption {
        default = {};
        description = "Rootless podman containers, managed as user-level quadlet units.";
        type = lib.types.attrsOf (lib.types.submodule {
          options = {
            image = lib.mkOption {type = lib.types.str;};
            ports = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [];
            };
            volumes = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [];
            };
            environment = lib.mkOption {
              type = lib.types.attrsOf lib.types.str;
              default = {};
            };
            restart = lib.mkOption {
              type = lib.types.str;
              default = "on-failure";
            };
          };
        });
      };
    };

    # All config is guarded: if no units are defined, cfg.user is never evaluated
    # so the throw default never fires when the module is imported without containers.
    config = lib.mkIf (cfg.units != {}) {
      assertions = [
        {
          assertion = config.users.users.${cfg.user}.uid != null;
          message = "features.containers.user (${cfg.user}) must have an explicit uid for quadlet support.";
        }
      ];

      environment.etc =
        lib.mapAttrs' (
          name: unit:
            lib.nameValuePair
            "containers/systemd/users/${toString config.users.users.${cfg.user}.uid}/${name}.container"
            {text = mkQuadlet unit;}
        )
        cfg.units;

      systemd.tmpfiles.rules =
        map (dir: "d ${dir} 0755 ${cfg.user} ${cfg.user} -") allDirs;

      # Required for rootless podman user units to find newuidmap/sh
      systemd.user.extraConfig = ''
        DefaultEnvironment="PATH=/run/current-system/sw/bin:/run/wrappers/bin"
      '';

      users.users.${cfg.user} = {
        linger = true;
        extraGroups = ["podman"];
        subUidRanges = [
          {
            startUid = 100000;
            count = 65536;
          }
        ];
        subGidRanges = [
          {
            startGid = 100000;
            count = 65536;
          }
        ];
      };

      users.groups.podman = {};
    };
  };
}
