{ inputs }:
let
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.nixlib.lib) concatPaths deepMerge;

  # `let` is recursive, so own and lib' resolve as a fixpoint. A lib file may
  # use lib' inside a function body, but not at definition time.
  own =
    builtins.foldl' (acc: path: acc // import path { inherit inputs lib lib'; }) { }
      (concatPaths {
        paths = ./.;
        exclude = [ ./default.nix ];
        filterDefault = false;
      });

  # own is merged with // rather than deepMerge: deepMerge forces each value to
  # test for an attrset, which would run vault's fetchGit on every eval. No
  # top-level name in own collides with the base libs.
  lib' =
    deepMerge [
      lib
      inputs.home-manager.lib
      inputs.nixlib.lib
    ]
    // own;
in
{
  # own goes to flake.lib, lib' to every module as an argument.
  inherit own lib';
}
