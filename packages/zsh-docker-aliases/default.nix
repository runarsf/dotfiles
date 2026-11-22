_: {
  perSystem = { pkgs, ... }: {
    packages.zsh-docker-aliases = pkgs.fetchFromGitHub { # follow:branch master
      owner = "akarzim";
      repo = "zsh-docker-aliases";
      rev = "77716803ac38ed0ab414c71ff25c8f9565ec2849";
      sha256 = "1cqllwld31dhlf78jn58js1j3z110fbiys16d8scx4i9pvx0dxwy";
    };
  };
}
