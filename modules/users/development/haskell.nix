{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config "dev.haskell" {
  home.packages = [
    (pkgs.haskellPackages.ghcWithPackages
      (pkgs: with pkgs; [stack cabal-install]))
  ];
}
