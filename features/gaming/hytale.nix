{inputs, ...}: {
  flake.homeModules.hytale = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    home.packages = [
      inputs.hytale-launcher.packages.${system}.default
    ];
  };
}
