{ config, pkgs, ... }:

{
  imports = [
    ./json2nix.nix
  ];

  home.sessionVariables = {
    FLAKE = "${config.home.homeDirectory}/.config/nixos";
  };

  programs.nix-index-database.comma.enable = true;

  home = {
    packages = with pkgs; [
      nil
      nixfmt
      cached-nix-shell
      deadnix
      statix

      nh
      nix-output-monitor
      nvd
      nix-tree
      manix
    ];
  };
}
