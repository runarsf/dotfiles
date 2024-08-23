{
  config,
  pkgs,
  outputs,
  ...
}:

{
  imports = [ ./fonts.nix ];
}
// outputs.lib.mkDesktopModule config "writing" {
  # FIXME This needs to be added to lib/default-values.nix because the sets don't get merged
  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];

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

  home.packages = with pkgs.unstable; [
    obsidian
    libreoffice-fresh
    typst
    pandoc
    poppler_utils
    sc-im
    anki
  ];
}
