{ config, pkgs, inputs, outputs, ... }:

outputs.lib.mkDesktopModule config "audiorelay" {
  nixpkgs.overlays = [ inputs.stackpkgs.overlays.default ];

  nixos.networking.firewall = {
    allowedUDPPorts = [ 59100 59200 ];
    allowedTCPPorts = [ 59100 ];
  };

  home.packages = with pkgs; [ stackpkgs.audiorelay ];
}
