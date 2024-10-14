{ config, outputs, pkgs, ... }:

outputs.lib.mkModule config "python-manim" {
  home.packages = with pkgs.python311Packages;
    [
      manim
    ];
}
