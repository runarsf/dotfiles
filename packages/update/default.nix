{lib, ...}: let
  releaseLockedInputs = [
    "hyprland"
    "hyprland-plugins"
    "dms"
    "vicinae"
    "nwg-displays"
    "zed"
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
    lockedStr = concatStringsSep "," releaseLockedInputs;
  in {
    packages = {
      inherit update-flake;
      update = writeNuBin "update" ''
        # An `update-flake` wrapper.
        def main [
          ...inputs: string,  # specific inputs to update; empty updates all
          --no-fetchgit,      # skip fetchgit update (runs by default)
          --no-release,       # skip release-locked inputs
        ] {
          let dir_args = if "NH_FLAKE" in $env { ["--dir" $env.NH_FLAKE] } else { [] }
          let fetchgit_args = if $no_fetchgit { [] } else { ["--fetchgit"] }
          let all_args = if ($inputs | is-empty) { ["--all"] } else { [] }
          let release_args = if $no_release { [] } else { ["--release" "${lockedStr}"] }
          ^${update-flake}/bin/update-flake ...$dir_args ...$fetchgit_args ...$all_args ...$release_args ...$inputs
        }
      '';
    };
  };
}
