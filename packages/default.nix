{ pkgs ? import <nixpkgs> { } }:

let
  pyinstaller = pkgs.callPackage ./pyinstaller.nix { };

in with pkgs; {
  niks = callPackage ./niks { };
  pixelflasher = callPackage ./pixelflasher.nix { inherit pyinstaller; };
  polycat = callPackage ./polycat.nix { };
  creep2 = callPackage ./creep2.nix { };
}
