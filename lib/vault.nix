_: {
  vaultPath = fetchGit { # follow:branch main
    url = "ssh://git@github.com/runarsf/vault";
    ref = "refs/heads/main";
    rev = "afa21f42c0c641d51dd5ba453bc9684e583879e9";
  };
}
