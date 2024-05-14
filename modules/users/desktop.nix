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
    piper
  ];

  nixos.services.ratbagd.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/Music";
  };
}
