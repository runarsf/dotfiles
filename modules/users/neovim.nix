{ pkgs, inputs, ... }:

{
  nixpkgs.overlays = [
    (_: prev: {
      neovim = inputs.nixvim.packages.${prev.system}.default;
    })
  ];

  home.packages = with pkgs; [ neovim ];

  home = {
    sessionVariables = {
      EDITOR = "nvim";
      GIT_EDITOR = "nvim";
      VISUAL = "nvim";
      DIFFPROG = "nvim -d";
      MANPAGER = "nvim +Man!";
      MANWIDTH = 999;
    };
    shellAliases.vim = "${pkgs.neovim}/bin/nvim";
  };
}
