{ config, pkgs, outputs, ... }:

outputs.lib.mkModule config "dev.rust" {
  home.packages = with pkgs; [
    cargo
    rustc
  ];

  PATH = [ "${config.home.homeDirectory}/.cargo/bin" ];
}
