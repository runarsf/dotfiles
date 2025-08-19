{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "mullvad" {
  nixos = {
    # https://mullvad.net/en/help/tag/connectivity#39
    networking.firewall = {
      allowedTCPPorts = [80 443 1401];
      allowedUDPPorts = [53 1194 1195 1196 1197 1300 1301 1302 1303 1400];
    };
    services = {
      mullvad-vpn = {
        enable = true;
        package = pkgs.mullvad-vpn;
      };
      # NOTE: https://discourse.nixos.org/t/connected-to-mullvadvpn-but-no-internet-connection/35803/7?u=lion
      # resolved.enable = true;
    };
    # networking.resolvconf.enable = false;
  };
}
