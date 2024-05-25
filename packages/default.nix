{ pkgs ? import <nixpkgs> { } }:

let
  pyinstaller = pkgs.callPackage ./pyinstaller { };

in {
  niks = pkgs.callPackage ./niks { };
  pixelflasher = pkgs.callPackage ./pixelflasher { inherit pyinstaller; };
}
