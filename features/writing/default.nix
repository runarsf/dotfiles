_: {
  flake.homeModules.writing = {pkgs, ...}: {
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
      libreoffice-fresh
      pandoc
      poppler-utils
      sc-im
      anki
      plantuml
      graphviz
      octaveFull
    ];
  };
}
