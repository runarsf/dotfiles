{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "writing" {
  # FIXME This needs to be added to lib/default-values.nix because the sets don't get merged
  nixpkgs.config.permittedInsecurePackages = ["electron-25.9.0"];

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

  nixpkgs.config.zathura.useMupdf = false;

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
}
