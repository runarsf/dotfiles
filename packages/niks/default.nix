{ pkgs, ... }:

# TODO writeShellApplication
# TODO Use cached-nix-shell --wrap to allow using just nix-shell in the script
pkgs.writeShellScriptBin "niks" (builtins.readFile ./niks.sh)
