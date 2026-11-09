_: {
  perSystem = { pkgs, ... }: {
    packages.zsh-fz = pkgs.fetchFromGitHub {
      owner = "johannjhang";
      repo = "fz.sh";
      rev = "fa41aa22abac86db1d1e9dd858d689d1c009aefa";
      sha256 = "sha256-M//pvh8WS0gsypfJzC1LRb7qdCxIdZWCgpIyh3/awh4=";
    };
  };
}
