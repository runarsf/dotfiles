# sudo systemctl --user -M blahaj@ status podman-<container>
# sudo -u blahaj journalctl --user -xeu podman-<container>
{
  config,
  pkgs,
  outputs,
  name,
  ...
}: let
  cfg = config.modules.containers;
in
  {
    imports = outputs.lib.concatPaths {
      paths = ./.;
      # TODO: Not sure if this is necessary
      exclude = ./default.nix;
    };
  }
  // outputs.lib.mkModule config "containers" {
    options' = with outputs.lib;
    with types; {
      username = mkOption {
        type = str;
        default = config.home.username;
        description = "Default username for containers.";
      };
      dirs = {
        data = mkOption {
          type = str;
          default = "/data";
          description = "Base data directory.";
        };
        containers = mkOption {
          type = str;
          default = "${cfg.dirs.data}/containers";
          description = "Data directory for containers.";
        };
        media = mkOption {
          type = str;
          default = "${cfg.dirs.data}/media";
          description = "Media data directory.";
        };
      };
    };

    config = let
      defaultDataDirs = with cfg.dirs; [
        data

        containers

        media
        "${media}/music"
        "${media}/movies"
        "${media}/series"
        "${media}/photos"
        "${media}/downloads"
        "${media}/downloads/music"
        "${media}/downloads/movies"
        "${media}/downloads/series"
        "${media}/downloads/photos"
      ];
      # Extract host path from volume string (format: "host:container" or "host:container:options")
      getHostPath = volumeStr:
        outputs.lib.head (outputs.lib.splitString ":" volumeStr);

      # Generate all parent paths for a given path
      # Example: "/path/to/container" -> ["/path" "/path/to" "/path/to/container"]
      getAllPathComponents = path: let
        # Split path and remove empty strings
        parts = outputs.lib.filter (p: p != "") (outputs.lib.splitString "/" path);

        # Generate cumulative paths
        # ["path" "to" "container"] -> ["/path" "/path/to" "/path/to/container"]
        generatePaths = parts: let
          folder = n: "/" + outputs.lib.concatStringsSep "/" (outputs.lib.take n parts);
        in
          map folder (outputs.lib.range 1 (outputs.lib.length parts));
      in
        if path == "" || path == "/"
        then []
        else generatePaths parts;

      # Check if a path should have a tmpfiles rule (filter out non-directories, /nix/store paths, etc.)
      shouldCreateTmpfilesRule = path:
        ! outputs.lib.hasPrefix "/nix/store" path
        && ! outputs.lib.hasPrefix "/run/secrets" path
        && (outputs.lib.hasPrefix cfg.dirs.data path);

      # Extract all volumes from all podman containers
      allPodmanVolumes = outputs.lib.flatten (
        outputs.lib.mapAttrsToList (
          _: containerConfig:
            containerConfig.volumes or []
        ) (config.services.podman.containers or {})
      );

      # Get host paths that need tmpfiles rules
      relevantHostPaths = map getHostPath (
        outputs.lib.filter (vol: shouldCreateTmpfilesRule (getHostPath vol)) allPodmanVolumes
      );

      # Get all path components (including parents) for each relevant path
      allPathsWithParents = outputs.lib.flatten (
        map getAllPathComponents (relevantHostPaths ++ defaultDataDirs)
      );

      # Get unique paths and sort them (shorter paths first, so parents are created before children)
      uniqueHostPaths = outputs.lib.unique (outputs.lib.naturalSort allPathsWithParents);
    in {
      nixos = {
        virtualisation = {
          containers = {
            enable = true;
            storage.settings = {
              storage = {
                runroot = "/run/containers/storage";
                graphroot = "/var/lib/containers/storage";
                options.overlay.mountopt = "nodev,metacopy=on";
              };
            };
          };

          # systemd unit name: podman-<service>
          # journalctl -u podman-<service>.service
          oci-containers.backend = "podman";

          # podman = {
          #   dockerSocket.enable = !virtualisation.docker.enable;
          # };
        };

        # Use Z only if you need to fix existing content
        # "Z /data 0775 media media -"
        # $ systemd-tmpfiles --tldr
        systemd.tmpfiles.rules = map (dir: "d ${dir} 0755 ${name} ${name} -") uniqueHostPaths;

        # Add 'newuidmap' and 'sh' to the PATH for users' Systemd units.
        # Required for Rootless podman.
        systemd.user.extraConfig = ''
          DefaultEnvironment="PATH=/run/current-system/sw/bin:/run/wrappers/bin:${outputs.lib.makeBinPath [pkgs.bash]}"
        '';

        environment.systemPackages = with pkgs; [
          passt
        ];

        users.users."${name}" = {
          linger = true;
          extraGroups = ["podman"];
          subUidRanges = [
            {
              startUid = 100000;
              count = outputs.lib.math.pow 2 16;
            }
          ];
          subGidRanges = [
            {
              startGid = 100000;
              count = outputs.lib.math.pow 2 16;
            }
          ];
        };
        users.groups."podman" = {
          name = "podman";
        };
      };
    };
  }
