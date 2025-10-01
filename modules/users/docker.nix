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

        # TODO: Allow rootless to run on port 80 and 443
        # rootless = {
        #   enable = true;
        #   setSocketVariable = true;
        # };

        autoPrune = {
          enable = true;
          dates = "03:30";
        };
      };

      oci-containers.backend = outputs.lib.mkDefault "docker";
    };

    networking.firewall.trustedInterfaces = [ "docker0" ];

    users.users."${name}".extraGroups = [ "docker" ];

    environment.systemPackages = with pkgs; [ docker-buildx ];
  };

  home.sessionVariables.COMPOSE_BAKE = "true";
}
