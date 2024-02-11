{ pkgs, ... }:

{
  home.packages = with pkgs; [
    bitwarden
    spotify
    qalculate-gtk
  ];
}
