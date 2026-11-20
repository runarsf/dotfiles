_: {
  flake.homeModules.writing = { pkgs, ... }: {
    programs.zathura = {
      enable = true;
      package = pkgs.zathura;
      options = {
        recolor = true;
      };
      mappings = {
        "<C-i>" = "recolor";
      };
    };

    home.packages = with pkgs; [
      obsidian
      typst
      libreoffice-stable
      pandoc
      poppler-utils
      sc-im
      plantuml
      graphviz
      octaveFull
    ];
  };
}
