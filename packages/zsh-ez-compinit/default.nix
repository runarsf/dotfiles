_: {
  perSystem = { pkgs, ... }: {
    packages.zsh-ez-compinit = pkgs.fetchFromGitHub {
      owner = "mattmc3";
      repo = "ez-compinit";
      rev = "5c9b6da88a463573e26e6846ff164026640ef2f6";
      sha256 = "sha256-j/HF2hwBHvNNQpXSyEswCLZIPk8Ee+goB8fC3FALqgo=";
    };
  };
}
