_: {
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) fetchFromGitHub;
    in
    {
      packages.game-controller-db = fetchFromGitHub { # follow:branch master
        owner = "mdqinc";
        repo = "SDL_GameControllerDB";
        rev = "4e87e3c6c2574f49e1cdf8526b6f829b61e5ccb3";
        hash = "sha256-63lFf9VhhdHH9Cak4JaJWeZHr1N0bO2QawB2Oemg7uA=";
      };
    };
}
