{ pkgs, ... }:

# TODO writeShellApplication
pkgs.writeShellScriptBin "niks" (builtins.readFile ./niks.sh)
