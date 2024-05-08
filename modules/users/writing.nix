{ pkgs, ... }:

{
  imports = [ ./fonts.nix ];

  # FIXME This needs to be added to lib/default-values.nix because the sets don't get merged
  nixpkgs.config.permittedInsecurePackages = [
    "electron-25.9.0"
  ];

  home.packages = with pkgs.unstable; [ obsidian libreoffice-fresh typst pandoc poppler_utils sc-im anki ];
}
