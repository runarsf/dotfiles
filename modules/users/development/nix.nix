{ config, pkgs, outputs, ... }:

outputs.lib.mkModule config "nix" {
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
      # TODO https://github.com/viperML/nh?tab=readme-ov-file#nixos-module
      nh
      nix-inspect
      nix-output-monitor
      nvd
      nix-tree
      manix
    ];
  };
}
