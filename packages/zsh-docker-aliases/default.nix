_: {
  perSystem = {pkgs, ...}: {
    packages.zsh-docker-aliases = pkgs.fetchFromGitHub {
      owner = "akarzim";
      repo = "zsh-docker-aliases";
      rev = "6c3479abeef33362a7ec7e713a5a95f292fee9b9";
      sha256 = "0iycb90l83l09skcd2yy0kc112225psgd063cf950v7gjga7h46z";
    };
  };
}
