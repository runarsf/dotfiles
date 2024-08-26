{ config, pkgs, outputs, ... }:

# Sunshine: https://github.com/LongerHV/nixos-configuration/blob/master/modules/nixos/sunshine.nix

outputs.lib.mkDesktopModule config "steam" {
  home.packages = with pkgs; [ protontricks winetricks ];

  nixos.programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };
}
