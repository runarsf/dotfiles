{ config, outputs, ... }:

outputs.lib.mkDesktopModule config "marker" {
  nixos.virtualisation.oci-containers.containers.marker = {
    image = "docker.io/savatar101/marker-api:0.3";
    extraOptions = [ "--pull=newer" ];
    ports = [ "5001:8000" ];
  };
}
