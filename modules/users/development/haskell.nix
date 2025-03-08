{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config "dev.haskell" {
  PATH = ["${config.home.homeDirectory}/.local/bin"];

  # TODO Add user info
  # home.file.".stack/config.yaml".text = outputs.lib.generators.toYAML {
  #   templates = {
  #     params = {
  #       author-email = "";
  #       author-name = "";
  #       category = "";
  #       copyright = "";
  #       github-username = "";
  #     };
  #   };
  # };

  home.packages = [
    (pkgs.haskellPackages.ghcWithPackages
      (pkgs: with pkgs; [stack cabal-install]))
  ];
}
