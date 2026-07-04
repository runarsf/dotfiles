{self, ...}: {
  flake.nixosModules.devPodman = {config, ...}: {
    imports = [self.nixosModules.podman];

    virtualisation.podman.dockerCompat =
      !config.virtualisation.docker.enable;
  };

  flake.homeModules.devPodman = {pkgs, ...}: {
    home.packages = with pkgs; [
      podman-compose
      dive
      podman-tui
    ];
  };
}
