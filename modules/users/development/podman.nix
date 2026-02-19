{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config ["dev" "podman"] {
  nixos = {
    virtualisation = {
      podman = {
        dockerCompat = !config.modules.docker.enable;
      };
    };

    environment.systemPackages = with pkgs; [
      podman-compose

      dive
      podman-tui
    ];
  };
}
