{ config, pkgs, ... }:

{
  imports = [
    ./discord.nix
    ./xdg.nix
  ];

  home.packages = with pkgs; [
    networkmanagerapplet
    bitwarden
    qalculate-gtk
    spotify
  ];

  services.mpd = {
    enable = true;
    musicDirectory = "${config.home.homeDirectory}/Music";
  };
}
