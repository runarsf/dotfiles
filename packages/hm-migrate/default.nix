{ lib, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      inherit (builtins) readFile;
      inherit (lib) makeBinPath;
      inherit (pkgs) makeWrapper symlinkJoin;
      inherit (pkgs.writers) writeNuBin;
    in
    {
      packages.hm-migrate = symlinkJoin {
        name = "hm-migrate";
        paths = [ (writeNuBin "hm-migrate" (readFile ./bin/hm-migrate.nu)) ];
        nativeBuildInputs = [ makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/hm-migrate \
            --prefix PATH : ${
              makeBinPath [
                pkgs.coreutils
                pkgs.rsync
                pkgs.lsof
              ]
            }
        '';
      };
    };
}
