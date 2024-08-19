{ outputs, pkgs, config, ... }:

outputs.lib.mkModule config "javascript" {
  home.packages = with pkgs.unstable; [
    nodejs
  ];
}
