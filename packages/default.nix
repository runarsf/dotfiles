{ pkgs ? import <nixpkgs> { } }:

let
  pyinstaller = pkgs.callPackage ./pyinstaller { };

in with pkgs; {
  niks = callPackage ./niks { };
  pixelflasher = callPackage ./pixelflasher { inherit pyinstaller; };
  polycat = callPackage ./polycat { };
}
