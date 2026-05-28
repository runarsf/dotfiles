{lib, ...}: let
  releaseLockedInputs = [
    "hyprland"
    "dms"
    "vicinae"
    "vicinae"
    "nwg-displays"
  ];
in {
  perSystem = {pkgs, ...}: let
    inherit (builtins) readFile concatStringsSep;
    inherit (lib) makeBinPath;
    inherit (pkgs) makeWrapper symlinkJoin;
    inherit (pkgs.writers) writeNuBin;

    update-flake = symlinkJoin {
      name = "update-flake";
      paths = [(writeNuBin "update-flake" (readFile ./bin/update-flake.nu))];
      nativeBuildInputs = [makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/update-flake \
          --prefix PATH : ${makeBinPath (with pkgs; [update-nix-fetchgit fd])}
      '';
    };
    lockedNu = "[" + concatStringsSep ", " (map (x: "\"${x}\"") releaseLockedInputs) + "]";
  in {
    packages = {
      inherit update-flake;
      updater = writeNuBin "update-flake" ''
        let dir_args = if "NH_FLAKE" in $env { ["--dir" $env.NH_FLAKE] } else { [] }
        ^${update-flake}/bin/update-flake ...$dir_args --inputs --fetchgit ...${lockedNu}
      '';
    };
  };
}
