{ lib, ... }:
let
  # NOTE: hyprland's cache gets evicted on a schedule, so releases tend to not be cached
  releaseLockedInputs = [
    "dms"
    "vicinae"
    "nwg-displays"
    "zed"
  ];
in
{
  perSystem =
    { pkgs, ... }:
    let
      inherit (builtins) readFile concatStringsSep;
      inherit (lib) makeBinPath;
      inherit (pkgs) makeWrapper symlinkJoin;
      inherit (pkgs.writers) writeNuBin;

      update-flake = symlinkJoin {
        name = "update-flake";
        paths = [ (writeNuBin "update-flake" (readFile ./bin/update-flake.nu)) ];
        nativeBuildInputs = [ makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/update-flake \
            --prefix PATH : ${
              makeBinPath (
                with pkgs;
                [
                  update-nix-fetchgit
                  fd
                ]
              )
            }
        '';
      };
      lockedStr = concatStringsSep "," releaseLockedInputs;

      update = symlinkJoin {
        name = "update";
        paths = [
          (writeNuBin "update" ''
            # Prints anything tagged `# UPDATE:` for manual review.
            def audit [dir: string] {
              let root = ($dir | path expand)
              let marker = (do { ^rg --no-heading --line-number '^\s*# UPDATE:' $root } | complete)
              if $marker.exit_code == 0 {
                print ""
                print $"(ansi yellow_bold)info:(ansi reset) marked for manual review:"
                $marker.stdout
                | str trim
                | lines
                | each { |line|
                    let row = ($line | parse "{file}:{num}:{text}" | first)
                    let rel = ($row.file | path relative-to $root)
                    let text = ($row.text | str trim | str replace "# UPDATE:" "" | str trim)
                    print $"  (ansi cyan_bold)($rel)(ansi reset):(ansi green_bold)($row.num)(ansi reset)  ($text)"
                  }
                | ignore
              }
            }

            # An `update-flake` wrapper.
            def main [
              ...inputs: string,  # specific inputs to update; empty updates all
              --no-fetchgit,      # skip fetchgit update (runs by default)
              --no-release,       # skip release-locked inputs
              --audit,            # only print `# UPDATE:` markers, skip updating
            ] {
              let dir = if "NH_FLAKE" in $env { $env.NH_FLAKE } else { "." }

              if $audit {
                audit $dir
                return
              }

              let dir_args = if "NH_FLAKE" in $env { ["--dir" $env.NH_FLAKE] } else { [] }
              let fetchgit_args = if $no_fetchgit { [] } else { ["--fetchgit"] }
              let all_args = if ($inputs | is-empty) { ["--all"] } else { [] }
              let release_args = if $no_release { [] } else { ["--release" "${lockedStr}"] }
              ^${update-flake}/bin/update-flake ...$dir_args ...$fetchgit_args ...$all_args ...$release_args ...$inputs

              audit $dir
            }
          '')
        ];
        nativeBuildInputs = [ makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/update \
            --prefix PATH : ${makeBinPath [ pkgs.ripgrep ]}
        '';
      };
    in
    {
      packages = {
        inherit update-flake update;
      };
    };
}
