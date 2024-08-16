{ outputs, pkgs, config, ... }:

outputs.lib.mkModule config "Java" {
  programs.java = {
    enable = true;
    package = pkgs.unstable.jdk21;
  };
}