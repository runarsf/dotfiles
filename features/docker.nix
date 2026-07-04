{self, ...}: {
  flake.nixosModules.docker = {
    pkgs,
    config,
    ...
  }: {
    virtualisation = {
      docker = {
        enable = true;
        enableOnBoot = true;

        # rootless = {
        #   enable = true;
        #   setSocketVariable = true;
        # };

        autoPrune = {
          enable = true;
          dates = "03:30";
        };
      };
    };

    networking.firewall.trustedInterfaces = ["docker0"];

    users.groups.docker.members = config.primaryUsers;

    environment.systemPackages = with pkgs; [docker-buildx];

    environment.sessionVariables.COMPOSE_BAKE = "true";
  };
}
