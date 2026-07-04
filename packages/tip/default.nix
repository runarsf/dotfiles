{lib, ...}: {
  perSystem = {pkgs, ...}: let
    inherit (builtins) readFile;
    inherit (lib) makeBinPath;
    inherit (pkgs) makeWrapper symlinkJoin;
    inherit (pkgs.writers) writeNuBin;
  in {
    packages.tip = symlinkJoin {
      name = "tip";
      paths = [(writeNuBin "tip" (readFile ./bin/tip.nu))];
      nativeBuildInputs = [makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/tip \
          --prefix PATH : ${makeBinPath (with pkgs; [git busybox])}
      '';
    };
  };
}
