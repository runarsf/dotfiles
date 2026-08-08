{lib', ...}: {
  perSystem = {pkgs, ...}: let
    resizebdf' = lib'.fonts.resizebdf pkgs;
    resizettf' = lib'.fonts.resizettf pkgs;
    renamettf' = lib'.fonts.renamettf pkgs;
  in {
    packages = {
      scientifica-hidpi = pkgs.scientifica.overrideAttrs (oldAttrs: let
        bitmap = "$out/share/fonts/misc/";
        truetype = "$out/share/fonts/truetype/";
      in {
        postInstall =
          oldAttrs.postInstall or ""
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
