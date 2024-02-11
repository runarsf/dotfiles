{ pkgs, outputs, inputs, ... }:

{
  nixpkgs.overlays = [
    (_: prev: {
      apple-fonts = import ./apple-fonts.nix {
        inherit (prev) stdenv fetchurl p7zip;
        lib = outputs.lib;
      };
    })
    (final: prev: {
      sf-mono-liga-bin = prev.stdenvNoCC.mkDerivation {
        pname = "sf-mono-liga-bin";
        version = "dev";
        src = inputs.sf-mono-liga-src;
        dontConfigure = true;
        installPhase = ''
          mkdir -p $out/share/fonts/opentype
          cp -R $src/*.otf $out/share/fonts/opentype/
        '';
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

   sf-mono-liga-bin

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
