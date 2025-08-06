{
  pkgs,
  config,
  outputs,
  ...
}:
outputs.lib.mkModule config "localsend" {
  home.packages = with pkgs; [ localsend ];

  nixos = {
    networking.firewall = {
      allowedTCPPorts = [
        53317
      ];
      allowedUDPPorts = [
        53317
      ];
    };
  };
}
