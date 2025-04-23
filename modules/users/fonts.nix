{
  pkgs,
  outputs,
  inputs,
  config,
  ...
}:
# TODO https://codeberg.org/adamcstephens/apple-fonts.nix/src/branch/main
let
  nofontsdir = font:
    font.overrideAttrs (oldAttrs: {
      installPhase =
        oldAttrs.installPhase
        + ''
          find "$out/share/fonts" -type f -name 'fonts.dir' -delete
        '';
    });
  resizebdf = pkgs.writers.writePython3Bin "resizebdf" {
    libraries = with pkgs.python3Packages; [numpy];
    flakeIgnore = [
      "E302"
      "W293"
      "E226"
      "E305"
      "E265" # from nix-shell shebang
      "E501" # line too long (82 > 79 characters)
      "F403" # ‘from module import *’ used; unable to detect undefined names
      "F405" # name may be undefined, or defined from star imports: module
    ];
  } (builtins.readFile ./resize_bdf.py);
in
  outputs.lib.mkDesktopModule config "fonts" {
    nixpkgs.overlays = [
      (_: prev: {
        scientifica-hidpi = pkgs.scientifica.overrideAttrs (oldAttrs: let
          base = "$out/share/fonts/misc/";
        in {
          installPhase =
            oldAttrs.installPhase
            + ''
              find "$out/share/fonts" -type f \( -name '*.ttf' -o -name '*.otb' \) -delete
              mv ${base}/scientifica-11.bdf ${base}/scientificaItalic-11.bdf ${base}/scientificaBold-11.bdf .
              ${resizebdf}/bin/resizebdf ./scientifica-11.bdf ${base}/scientifica-11.bdf 2
              ${resizebdf}/bin/resizebdf ./scientificaItalic-11.bdf ${base}/scientificaItalic-11.bdf 2
              ${resizebdf}/bin/resizebdf ./scientificaBold-11.bdf ${base}/scientificaBold-11.bdf 2
            '';
        });
        creep2-hidpi = pkgs.creep2.overrideAttrs (oldAttrs: let
          base = "$out/share/fonts/misc/";
        in {
          installPhase =
            oldAttrs.installPhase
            + ''
              mv ${base}/creep2-11.bdf .
              ${resizebdf}/bin/resizebdf ./creep2-11.bdf ${base}/creep2-11.bdf 2
            '';
        });
      })
    ];
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

    home.packages = with pkgs;
      [
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
        noto-fonts-cjk-sans
        noto-fonts-extra
        powerline-fonts

        # Bitmap fonts
        cozette
        undefined-medium
        scientifica-hidpi
        zpix-pixel-font
        termsyn
        terminus_font
        monocraft
        creep2-hidpi
        (nofontsdir tamzen)
        (nofontsdir proggyfonts)
        (nofontsdir unifont)
        (nofontsdir unifont_upper)
        (nofontsdir gohufont)
        (nofontsdir spleen)

        # Coding
        jetbrains-mono
        sudo-font
        cascadia-code
        # maple-mono.NF
        mplus-outline-fonts.githubRelease
      ]
      ++ (with pkgs.nerd-fonts; [
        caskaydia-cove
        caskaydia-mono
        comic-shanns-mono
        jetbrains-mono
        monaspace
        ubuntu
        ubuntu-mono
        commit-mono
        im-writing
        fira-code
        gohufont
        lilex
        noto
      ]);
  }
