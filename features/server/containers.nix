{ self, lib', ... }:
{
  flake.nixosModules.containers =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.features.containers;

      uid = toString config.users.users.${cfg.user}.uid;
      # Where podman's user generator looks for a rootless user's quadlets.
      unitDir = "containers/systemd/users/${uid}";

      hostPath = vol: builtins.head (lib.splitString ":" vol);

      pathAncestors =
        path:
        let
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

      relevantHostPaths = map hostPath (
        lib.filter (v: lib.hasPrefix cfg.dirs.data (hostPath v)) allVolumes
      );

      allDirs = lib.unique (
        lib.naturalSort (lib.flatten (map pathAncestors (relevantHostPaths ++ defaultDirs)))
      );

      section = name: entries: "[${name}]\n" + lib.concatMapStrings (e: "${e}\n") entries;

      mkQuadlet =
        name: unit:
        let
          # A declared network is a quadlet unit of its own; anything else
          # (pasta, host, an existing network) passes through as given.
          network =
            if builtins.hasAttr unit.network cfg.networks then
              "${unit.network}.network"
            else
              unit.network;
        in
        lib.concatStringsSep "\n" [
          (section "Unit" [
            "Description=${unit.description}"
            # Pulls on start, so coming up before the network would otherwise
            # exhaust the default start limit and leave the unit dead.
            "StartLimitIntervalSec=0"
          ])
          (section "Container" (
            [
              # Quadlet would name it systemd-<unit>, which is also its DNS alias.
              "ContainerName=${name}"
              "Image=${unit.image}"
              "Pull=${unit.pull}"
              "AutoUpdate=${unit.autoUpdate}"
              "UserNS=${unit.userNS}"
              "Network=${network}"
            ]
            ++ map (g: "GroupAdd=${g}") unit.groupAdd
            ++ map (p: "PublishPort=${p}") unit.ports
            ++ map (v: "Volume=${v}") unit.volumes
            ++ lib.mapAttrsToList (k: v: "Environment=${k}=${v}") unit.environment
            ++ map (f: "EnvironmentFile=${f}") unit.environmentFiles
            ++ lib.optional (unit.exec != null) "Exec=${unit.exec}"
            ++ lib.optional (
              unit.podmanArgs != [ ]
            ) "PodmanArgs=${lib.concatStringsSep " " unit.podmanArgs}"
          ))
          (section "Service" [
            "Restart=${unit.restart}"
            "RestartSec=${toString unit.restartSec}"
          ])
          (section "Install" [ "WantedBy=default.target" ])
        ];

      mkNetworkUnit =
        name: net:
        lib.concatStringsSep "\n" [
          (section "Unit" [ "Description=${name} podman network" ])
          (section "Network" [
            "NetworkName=${name}"
            "Driver=${net.driver}"
          ])
          (section "Install" [ "WantedBy=default.target" ])
        ];

      # Unit names as podman's generator will create them, for the apply service.
      quadlets =
        lib.mapAttrsToList (name: unit: {
          unit = "${name}.service";
          text = mkQuadlet name unit;
        }) cfg.units
        ++ lib.mapAttrsToList (name: net: {
          unit = "${name}-network.service";
          text = mkNetworkUnit name net;
        }) cfg.networks;

      stateDir = "/var/lib/podman-quadlet/${cfg.user}";

      # "<unit> <hash>" per line; a file so the script needs no heredoc.
      declaredFile = pkgs.writeText "podman-quadlet-units" (
        lib.concatMapStrings (q: "${q.unit} ${builtins.hashString "sha256" q.text}\n") quadlets
      );

      applyScript = pkgs.writeShellScript "podman-quadlet-apply" ''
        set -u

        # -M <user>@.host reaches that user's own systemd instance.
        sc() {
          ${config.systemd.package}/bin/systemctl --user -M ${cfg.user}@.host "$@"
        }

        mkdir -p ${stateDir}

        reload=0
        changed=""

        while read -r unit hash; do
          [ -n "$unit" ] || continue
          if [ "$(cat ${stateDir}/"$unit" 2>/dev/null)" != "$hash" ]; then
            reload=1
            changed="$changed $unit"
          fi
        done < ${declaredFile}

        # Stop removed quadlets before the reload, while their units exist.
        for stamp in ${stateDir}/*.service; do
          [ -e "$stamp" ] || continue
          unit=$(basename "$stamp")
          if ! grep -q "^$unit " ${declaredFile}; then
            echo "quadlet $unit is no longer declared, stopping it"
            sc stop "$unit" || true
            rm -f "$stamp"
            reload=1
          fi
        done

        [ "$reload" = 1 ] || exit 0

        if ! sc daemon-reload; then
          echo "could not reach ${cfg.user}'s systemd instance, leaving the containers alone" >&2
          exit 1
        fi

        for unit in $changed; do
          echo "applying quadlet $unit"
          sc restart "$unit" || true
        done

        # Only once the reload went through, so a failed run is retried.
        while read -r unit hash; do
          [ -n "$unit" ] || continue
          printf '%s\n' "$hash" > ${stateDir}/"$unit"
        done < ${declaredFile}
      '';
    in
    {
      imports = [ (lib'.importFeature self "nixos" "podman") ];

      options.features.containers = {
        user = lib.mkOption {
          type = lib.types.str;
          default = throw "Set features.containers.user to the service user that runs containers";
          description = "System user that owns and runs the containers.";
        };
        account = lib.mkOption {
          type = lib.types.str;
          default = cfg.user;
          description = "Login name for the accounts the services keep themselves (slskd and copyparty web logins). Not a system user.";
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
        networks = lib.mkOption {
          default = { };
          description = "Podman networks as quadlet units. A unit whose `network` names one joins it, and reaches the others on it by unit name.";
          type = lib.types.attrsOf (
            lib.types.submodule {
              options.driver = lib.mkOption {
                type = lib.types.str;
                default = "bridge";
              };
            }
          );
        };
        units = lib.mkOption {
          default = { };
          description = "Rootless podman containers, managed as user-level quadlet units.";
          type = lib.types.attrsOf (
            lib.types.submodule (
              { name, ... }:
              {
                options = {
                  description = lib.mkOption {
                    type = lib.types.str;
                    default = "${name} container";
                  };
                  image = lib.mkOption { type = lib.types.str; };
                  ports = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ ];
                    description = "Published ports, as host:container.";
                  };
                  volumes = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ ];
                    description = "Bind mounts, as host:container[:options]. A host path under dirs.data is created for the container user first.";
                  };
                  environment = lib.mkOption {
                    type = lib.types.attrsOf lib.types.str;
                    default = { };
                    description = "Environment variables. These end up in the world-readable unit file, so secrets belong in environmentFiles.";
                  };
                  environmentFiles = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ ];
                    description = "Files to read environment variables from, typically sops templates.";
                  };
                  exec = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = null;
                    description = "Command line to run in the container, overriding the image's CMD.";
                  };
                  network = lib.mkOption {
                    type = lib.types.str;
                    default = "pasta";
                    description = "A name from features.containers.networks, or a podman network mode.";
                  };
                  userNS = lib.mkOption {
                    type = lib.types.str;
                    default = "keep-id";
                    description = "User namespace mode. keep-id:uid=0,gid=0 for images that insist on running as root inside.";
                  };
                  groupAdd = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ "keep-groups" ];
                  };
                  pull = lib.mkOption {
                    type = lib.types.str;
                    default = "newer";
                  };
                  autoUpdate = lib.mkOption {
                    type = lib.types.str;
                    default = "registry";
                  };
                  podmanArgs = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ ];
                    description = "Extra arguments passed to podman verbatim.";
                  };
                  restart = lib.mkOption {
                    type = lib.types.str;
                    default = "on-failure";
                  };
                  restartSec = lib.mkOption {
                    type = lib.types.int;
                    default = 10;
                    description = "Seconds to wait between restart attempts.";
                  };
                };
              }
            )
          );
        };
      };

      # All config is guarded: if no units are defined, cfg.user is never evaluated
      # so the throw default never fires when the module is imported without containers.
      config = lib.mkIf (cfg.units != { }) {
        assertions = [
          {
            assertion = config.users.users.${cfg.user}.uid != null;
            message = "features.containers.user (${cfg.user}) must have an explicit uid for quadlet support.";
          }
        ];

        environment.etc =
          lib.mapAttrs' (
            name: unit: lib.nameValuePair "${unitDir}/${name}.container" { text = mkQuadlet name unit; }
          ) cfg.units
          // lib.mapAttrs' (
            name: net: lib.nameValuePair "${unitDir}/${name}.network" { text = mkNetworkUnit name net; }
          ) cfg.networks;

        systemd.tmpfiles.rules = map (dir: "d ${dir} 0755 ${cfg.user} ${cfg.user} -") allDirs;

        # A switch rewrites the files under /etc but never touches the user
        # manager that turns them into units, so this reloads it and restarts
        # only the containers whose quadlet changed.
        systemd.services."podman-quadlet-${cfg.user}" = {
          description = "Apply ${cfg.user}'s podman quadlets to their systemd instance";
          wantedBy = [ "multi-user.target" ];
          wants = [ "user@${uid}.service" ];
          after = [ "user@${uid}.service" ];
          restartTriggers = map (q: q.text) quadlets;
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = applyScript;
          };
        };

        # Required for rootless podman user units to find newuidmap/sh
        systemd.user.settings.Manager.DefaultEnvironment = "PATH=/run/current-system/sw/bin:/run/wrappers/bin";

        users.users.${cfg.user} = {
          linger = true;
          extraGroups = [ "podman" ];
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

        users.groups.podman = { };
      };
    };
}
