{ config, pkgs, ... }:

{
  imports = [
    ./discord.nix
    ./xdg.nix
    # ./easyeffects
  ];

  home.packages = with pkgs; [
    networkmanagerapplet
    bitwarden
    qalculate-gtk
    spotify
    vlc
  ];

  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/Music";
  };
}
