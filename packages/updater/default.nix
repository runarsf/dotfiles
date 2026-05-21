_: let
  releaseLockedInputs = ["hyprland" "dms"];
in {
  perSystem = {pkgs, ...}: let
    update-release = pkgs.writers.writeNuBin "update-release" (builtins.readFile ./bin/update-release.nu);
    lockedNu = "[" + builtins.concatStringsSep ", " (map (x: "\"${x}\"") releaseLockedInputs) + "]";
  in {
    packages.updater = pkgs.writers.writeNuBin "flake-updater" ''
      ^${update-release}/bin/update-release --all ...${lockedNu}
    '';
  };
}
