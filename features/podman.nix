_: {
  flake.nixosModules.podman = {
    config,
    pkgs,
    ...
  }: {
    environment.systemPackages = with pkgs; [
      podman-compose
      passt
      dive
      oxker
    ];

    environment.sessionVariables.DOCKER_HOST = "unix://\${XDG_RUNTIME_DIR}/podman/podman.sock";

    boot.kernel.sysctl."net.ipv4.ip_unprivileged_port_start" = 0;

    virtualisation = {
      containers = {
        enable = true;
        registries.search = ["docker.io"];
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
        dockerCompat = !config.virtualisation.docker.enable;
      };
    };
  };
}
