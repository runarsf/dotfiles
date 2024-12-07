{ config, pkgs, outputs, ... }:

outputs.lib.mkModule config "rust" {
  home.packages = with pkgs; [
    cargo
    rustc
  ];
  PATH = [ "${config.home.homeDirectory}/.cargo/bin" ];
}
