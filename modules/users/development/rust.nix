{ config, pkgs, outputs, ... }:

outputs.lib.mkModule config "Rust" {
  home.packages = with pkgs; [
    cargo
    rustc
  ];
}