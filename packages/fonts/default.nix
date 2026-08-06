_: {
  perSystem = {pkgs, ...}: let
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
    } (builtins.readFile ./bin/resize_bdf.py);
    resizebdf' = "${resizebdf}/bin/resizebdf";

    resizettf = pkgs.writers.writePython3Bin "resizettf" {
      libraries = with pkgs.python3Packages; [fonttools];
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
    } (builtins.readFile ./bin/resize_ttf.py);
    resizettf' = "${resizettf}/bin/resizettf";

    renamettf = pkgs.writers.writePython3Bin "renamettf" {
      libraries = with pkgs.python3Packages; [fonttools];
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
    } (builtins.readFile ./bin/rename_ttf.py);
    renamettf' = "${renamettf}/bin/renamettf";
  in {
    packages = {
      scientifica-hidpi = pkgs.scientifica.overrideAttrs (oldAttrs: let
        bitmap = "$out/share/fonts/misc/";
        truetype = "$out/share/fonts/truetype/";
      in {
        installPhase =
          oldAttrs.installPhase
          + ''
            find "$out/share/fonts" -type f \( -name '*.otb' \) -delete
            mkdir -p ./tmp
            mv ${bitmap}/scientifica*.bdf \
               ${truetype}/scientifica*.ttf \
               ./tmp

            ${resizebdf'} ./tmp/scientifica-11.bdf ${bitmap}/scientifica-11.bdf 2
            ${resizebdf'} ./tmp/scientificaBold-11.bdf ${bitmap}/scientificaBold-11.bdf 2
            ${resizebdf'} ./tmp/scientificaItalic-11.bdf ${bitmap}/scientificaItalic-11.bdf 2

            ${renamettf'} ./tmp/scientifica.ttf ./tmp/scientifica_vector.ttf "ScientificaVector"
            ${renamettf'} ./tmp/scientificaBold.ttf ./tmp/scientificaBold_vector.ttf "ScientificaVector"
            ${renamettf'} ./tmp/scientificaItalic.ttf ./tmp/scientificaItalic_vector.ttf "ScientificaVector"

            ${resizettf'} ./tmp/scientifica_vector.ttf ${truetype}/scientifica_vector.ttf 2
            ${resizettf'} ./tmp/scientificaBold_vector.ttf ${truetype}/scientificaBold_vector.ttf 2
            ${resizettf'} ./tmp/scientificaItalic_vector.ttf ${truetype}/scientificaItalic_vector.ttf 2

            rm -rf ./tmp
          '';
      });
    };
  };
}
