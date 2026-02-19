{
  config,
  outputs,
  ...
}:
outputs.lib.mkModule' config "podman" (config.modules.dev.podman.enable || config.modules.containers.enable) {
  services.podman.enable = true;

  nixos = {
    # Allow podman to bind to ports below 1024.
    # Surprisingly, this is also needed to bind to ports *inside* containers.
    boot.kernel.sysctl."net.ipv4.ip_unprivileged_port_start" = 0;

    virtualisation = {
      podman = {
        enable = true;

        autoPrune.enable = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    };
  };
}
