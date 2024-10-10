{ config, outputs, pkgs, ... }:

outputs.lib.mkModule config "podman" {
  nixos = {
    virtualisation = {
      podman = {
        enable = true;
        dockerCompat = false;

        package = pkgs.unstable.podman;

        defaultNetwork.settings.dns_enabled = true;
      };

      # systemd unit name: podman-<service>
      # journalctl -u podman-<service>.service
      oci-containers.backend = "podman";
    };

    environment.systemPackages = with pkgs.unstable; [ podman-compose ];
  };
}
