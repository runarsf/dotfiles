{ config, pkgs, outputs, ... }:

# Sunshine: https://github.com/LongerHV/nixos-configuration/blob/master/modules/nixos/sunshine.nix

# TODO: https://journix.dev/posts/gaming-on-nixos/
outputs.lib.mkDesktopModule config "steam" {
  home.packages = with pkgs; [ protontricks winetricks gamescope ];

  nixos.programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
    gamemode = {
      enable = true;
      enableRenice = true;
      settings = {
        general = {
          softrealtime = "auto";
          renice = 10;
        };
        custom = {
          start = "notify-send -a 'Gamemode' 'Optimizations activated'";
          end = "notify-send -a 'Gamemode' 'Optimizations deactivated'";
        };
      };
    };
  };
}
