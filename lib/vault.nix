_: {
  vaultPath = fetchGit {
    url = "ssh://git@github.com/runarsf/vault";
    ref = "refs/heads/main";
    rev = "b90674bfc9bdd76742be470d947171267876a07d";
  };
}
