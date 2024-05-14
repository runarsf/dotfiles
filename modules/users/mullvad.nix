{ pkgs, ... }:

{
  nixos = {
    environment.systemPackages = with pkgs; [
      mullvad-vpn
    ];
    networking.firewall = {
      allowedTCPPorts = [ 80 443 1401 ];
      allowedUDPPorts = [ 53 1194 1195 1196 1197 1300 1301 1302 1303 1400 ];
    };
    services.mullvad-vpn = {
      enable = true;
      enableExcludeWrapper = true;
    };
  };
}
