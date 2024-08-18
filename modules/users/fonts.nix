{
  pkgs,
  outputs,
  inputs,
  config,
  ...
}:

# TODO https://codeberg.org/adamcstephens/apple-fonts.nix/src/branch/main

outputs.lib.mkDesktopModule config "fonts" {
  nixos = {
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
    maple-mono-NF
    mplus-outline-fonts.githubRelease

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
        "IBMPlexMono"
        "FiraCode"
        "Hasklig"
        "Gohu"
        "iA-Writer"
        "Lilex"
        "Noto"
        "Terminus"
        "Agave"
      ];
    })
  ];
}
