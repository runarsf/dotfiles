{lib, ...}: {
  perSystem = {pkgs, ...}: let
    inherit (builtins) readFile;
    inherit (lib) makeBinPath;
    inherit (pkgs) makeWrapper symlinkJoin;
    inherit (pkgs.writers) writeNuBin;
  in {
    packages.cache-check = symlinkJoin {
      name = "cache-check";
      paths = [(writeNuBin "cache-check" (readFile ./bin/cache-check.nu))];
      nativeBuildInputs = [makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/cache-check \
          --prefix PATH : ${makeBinPath [pkgs.nix]}
      '';
    };
  };
}
