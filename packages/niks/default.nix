{lib, ...}: {
  perSystem = {pkgs, ...}: let
    inherit (builtins) readFile;
    inherit (lib) makeBinPath;
    inherit (pkgs) makeWrapper symlinkJoin;
    inherit (pkgs.writers) writeNuBin;
  in {
    packages.niks = symlinkJoin {
      name = "niks";
      paths = [(writeNuBin "niks" (readFile ./bin/niks.nu))];
      nativeBuildInputs = [makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/niks \
          --prefix PATH : ${makeBinPath (with pkgs; [nh git])}
      '';
    };
  };
}
