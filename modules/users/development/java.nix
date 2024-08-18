{ outputs, pkgs, config, ... }:

outputs.lib.mkModule config "java" {
  programs.java = {
    enable = true;
    package = pkgs.unstable.jdk21;
  };
}