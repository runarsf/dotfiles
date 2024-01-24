{ pkgs, ... }:

{
  nix = {
    package = pkgs.nix;

    settings = {
      auto-optimise-store = true;
      warn-dirty = false;

      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
    };
  };
}
