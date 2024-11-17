{ config, pkgs, inputs, outputs, ... }:

outputs.lib.mkDesktopModule config "hyprpanel" {
  nixpkgs.overlays = [ inputs.hyprpanel.overlay ];

  home.packages = with pkgs; [ hyprpanel ];
}
