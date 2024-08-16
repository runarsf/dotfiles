{ config, outputs, pkgs, ... }:

outputs.lib.mkModule config "Haskell" {
  home.packages = with pkgs; [
    ghc
    cabal-install
  ];
}