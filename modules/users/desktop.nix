{ config, pkgs, ... }:

{
  imports = [
    ./discord.nix
    # ./signal.nix
    ./xdg.nix
    ./spotify.nix
    # ./easyeffects
  ];

  home.packages = with pkgs; [
    networkmanagerapplet
    bitwarden
    qalculate-gtk
    vlc
    piper
  ];

  nixos.services.ratbagd.enable = true;

  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/Music";
  };
}
