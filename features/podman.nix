_: {
  flake.nixosModules.podman = {pkgs, ...}: {
    environment.systemPackages = [pkgs.passt];

    boot.kernel.sysctl."net.ipv4.ip_unprivileged_port_start" = 0;

    virtualisation = {
      containers = {
        enable = true;
        storage.settings.storage = {
          runroot = "/run/containers/storage";
          graphroot = "/var/lib/containers/storage";
          options.overlay.mountopt = "nodev,metacopy=on";
        };
      };

      oci-containers.backend = "podman";

      podman = {
        enable = true;
        autoPrune.enable = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };
  };
}
