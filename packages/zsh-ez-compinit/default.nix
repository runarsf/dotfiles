_: {
  perSystem = { pkgs, ... }: {
    packages.zsh-ez-compinit = pkgs.fetchFromGitHub { # follow:branch main
      owner = "mattmc3";
      repo = "ez-compinit";
      rev = "5c9b6da88a463573e26e6846ff164026640ef2f6";
      hash = "sha256-j/HF2hwBHvNNQpXSyEswCLZIPk8Ee+goB8fC3FALqgo=";
    };
  };
}
