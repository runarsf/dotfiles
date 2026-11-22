_: {
  perSystem = { pkgs, ... }: {
    packages.stderred = pkgs.stderred.overrideAttrs {
      src = pkgs.fetchFromGitHub { # follow:branch master
        owner = "sickill";
        repo = "stderred";
        rev = "cf0d118440a930024b4a58a9d787debc492256b7";
        sha256 = "1hix3pi7fmpna4swcgl87wx361c34nc4qisqkc6dn43vvxsilx2j";
      };
    };
  };
}
