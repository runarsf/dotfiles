{ pkgs, ... }:

{
  imports = [
    ./json2nix.nix
  ];

  programs.nix-index-database.comma.enable = true;

  home = {
    packages = with pkgs; [
      nil
      nixfmt
      deadnix
      statix
      manix
      nix-tree
      nix-output-monitor
    ];
  };
}
