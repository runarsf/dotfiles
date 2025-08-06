{
  config,
  pkgs,
  inputs,
  outputs,
  ...
}:
outputs.lib.mkModule config "neovim" rec {
  nixpkgs.overlays = [
    (_: prev: {
      neovim = inputs.nixvim.packages.${prev.system}.default;
    })
  ];

  home = let
    nvim = "${pkgs.neovim}/bin/nvim";
  in {
    packages = with pkgs; [neovim];
    sessionVariables = {
      EDITOR = "${nvim}";
      GIT_EDITOR = "${nvim}";
      VISUAL = "${nvim}";
      DIFFPROG = "${nvim} -d";
      MANPAGER = "${nvim} +Man!";
      MANWIDTH = 999;
    };
    shellAliases = {
      vim = "${nvim}";
      nv = "nix run path:${config.home.homeDirectory}/Development/nixvim --";
    };
  };

  programs.nushell.shellAliases = {
    inherit (home.shellAliases) vim;
  };
}
