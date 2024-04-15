{ pkgs, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;

      package = pkgs.unstable.docker;

      rootless.enable = true;

      autoPrune = {
        enable = true;
        dates = "03:30";
      };
    };
  };

  networking.firewall.trustedInterfaces = [ "docker0" ];

  environment.systemPackages = with pkgs.unstable; [ docker docker-compose ];
}
