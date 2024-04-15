{ pkgs, ... }:

{
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

  # users.users.ops.extraGroups = [ "docker" ];

  environment.systemPackages = with pkgs.unstable; [ podman podman-compose ];
}
