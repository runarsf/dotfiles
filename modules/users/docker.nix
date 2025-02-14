{
  config,
  outputs,
  pkgs,
  name,
  ...
}:
outputs.lib.mkModule config "docker" {
  nixos = {
    virtualisation = {
      docker = {
        enable = true;
        enableOnBoot = true;

        rootless = {
          enable = true;
          setSocketVariable = true;
        };

        autoPrune = {
          enable = true;
          dates = "03:30";
        };
      };

      oci-containers.backend =
        outputs.lib.mkIf (!config.nixos.virtualisation.podman.enable) "docker";
    };

    networking.firewall.trustedInterfaces = ["docker0"];

    users.users."${name}".extraGroups = ["docker"];

    environment.systemPackages = with pkgs.unstable; [podman-compose];
  };
}
