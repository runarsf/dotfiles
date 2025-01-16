{ outputs, pkgs, config, ... }:

outputs.lib.mkModule config "dev.javascript" {
  home.packages = with pkgs; [
    nodejs
  ];
}
