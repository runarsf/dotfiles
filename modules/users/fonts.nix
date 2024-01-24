{ pkgs, outputs, ... }:

{
  nixpkgs.overlays = [
    (_: prev: {
      apple-fonts = import ./apple-fonts.nix {
        inherit (prev) stdenv fetchurl p7zip;
        lib = outputs.lib;
      };
    })
  ];

  system = {
    fonts.fontconfig = {
      enable = true;
      hinting = {
        style = "slight";
        autohint = true;
      };
      subpixel = {
        lcdfilter = "default";
        rgba = "rgb";
      };
    };
  };

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
   fontpreview

    apple-fonts

    # Writing
    libertine
    atkinson-hyperlegible
    montserrat
    roboto
    ia-writer-duospace

    # Unicode table
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk
    noto-fonts-extra

    # Bitmap fonts
    unstable.cozette
    tamzen
    undefined-medium

    # Coding
    jetbrains-mono
    sudo-font
    cascadia-code

    # https://github.com/NixOS/nixpkgs/blob/nixpkgs-unstable/pkgs/data/fonts/nerdfonts/shas.nix
    (nerdfonts.override {
      fonts = [
        "CascadiaCode"
        "CascadiaMono"
        "ComicShannsMono"
        "JetBrainsMono"
        "Monaspace"
        "UbuntuMono"
        "CommitMono"
        "Gohu"
        "iA-Writer"
        "Lilex"
        "Noto"
        "Terminus"
      ];
    })
  ];
}
