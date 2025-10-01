{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config "podman" {
  nixos = {
    virtualisation = {
      podman = {
        enable = true;
        dockerCompat = false;

        defaultNetwork.settings.dns_enabled = true;
      };

      # systemd unit name: podman-<service>
      # journalctl -u podman-<service>.service
      oci-containers.backend = outputs.lib.mkForce "podman";
    };

    environment.systemPackages = with pkgs; [podman-compose];
  };
}
