{ config, outputs, pkgs, ... }:

outputs.lib.mkModule config "haskell" {
  home.packages = with pkgs; [
    ghc
    cabal-install
  ];
}