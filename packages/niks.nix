{ pkgs, ... }:

pkgs.writeShellScriptBin "niks" (builtins.readFile ./niks.sh)
