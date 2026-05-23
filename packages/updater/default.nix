{lib, ...}: let
  releaseLockedInputs = ["hyprland" "dms"];
in {
  perSystem = {pkgs, ...}: let
    inherit (builtins) readFile concatStringsSep;
    inherit (lib) makeBinPath;
    inherit (pkgs) makeWrapper symlinkJoin;
    inherit (pkgs.writers) writeNuBin;

    update-release = symlinkJoin {
      name = "update-release";
      paths = [(writeNuBin "update-release" (readFile ./bin/updater.nu))];
      nativeBuildInputs = [makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/update-release \
          --prefix PATH : ${makeBinPath (with pkgs; [update-nix-fetchgit fd])}
      '';
    };
    lockedNu = "[" + concatStringsSep ", " (map (x: "\"${x}\"") releaseLockedInputs) + "]";
  in {
    packages.updater = writeNuBin "flake-updater" ''
      let dir_args = if "NH_FLAKE" in $env { ["--dir" $env.NH_FLAKE] } else { [] }
      ^${update-release}/bin/update-release ...$dir_args --inputs --fetchgit ...${lockedNu}
    '';
  };
}
