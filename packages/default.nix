{ pkgs ? import <nixpkgs> { } }:

let
  pyinstaller = pkgs.callPackage ./pyinstaller { };

in with pkgs; {
  niks = callPackage ./niks { };
  # monolisa = callPackage ./monolisa { };
  pixelflasher = callPackage ./pixelflasher { inherit pyinstaller; };
}
