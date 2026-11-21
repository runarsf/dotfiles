_: {
  perSystem = { pkgs, ... }: {
    packages.zsh-fz = pkgs.fetchFromGitHub { # follow:branch master
      owner = "johannjhang";
      repo = "fz.sh";
      rev = "fa41aa22abac86db1d1e9dd858d689d1c009aefa";
      hash = "sha256-M//pvh8WS0gsypfJzC1LRb7qdCxIdZWCgpIyh3/awh4=";
    };
  };
}
