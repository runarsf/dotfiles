{ config, pkgs, inputs, ... }:

{
  # programs.neovim = {
  #   enable = true;
  #   viAlias = true;
  #   vimAlias = true;
  #   package = pkgs.neovim;
  # };

  home.shellAliases.vim = "${pkgs.neovim}/bin/nvim";

  home.packages = with pkgs; [ neovim ];

  nixpkgs = {
    overlays = [
      (_: prev: {
        neovim = inputs.nixvim.packages.${prev.system}.default;
      })
    ];
  };
}
