_: {
  perSystem = { pkgs, ... }: {
    packages.zsh-history-on-success = pkgs.fetchFromGitHub {
      owner = "nyoungstudios";
      repo = "zsh-history-on-success";
      rev = "f3a7d6f4dbfd19db1c4750191c6279ae9cd9812b";
      sha256 = "sha256-JTpgoKt6xkr5JbPTammNpH8Uo3nhUPT9rZvOnCMMIrI=";
    };
  };
}
