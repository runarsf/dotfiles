{ pkgs, ... }:

{
  home.packages = with pkgs; [
    bitwarden
    spotify
    firefox
    qalculate-gtk
  ];
}
