{ pkgs, ... }:

{
  programs.nm-applet.enable = true;
  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openconnect
    ];
  };
}
