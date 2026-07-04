_: {
  perSystem = {pkgs, ...}: let
    inherit (pkgs) fetchFromGitHub;
  in {
    packages.game-controller-db = fetchFromGitHub {
      owner = "mdqinc";
      repo = "SDL_GameControllerDB";
      rev = "992a0caf690e32a332a9707c355a4444516a2764";
      sha256 = "sha256-hv1xtAXpSQlzO1nSUkFaeoth4o0V7aUjzZgqnehezaY=";
    };
  };
}
