#!/usr/bin/env nu

# Scans direct top-level inputs (via root node) for any that follow `input_name`,
# returning explicit --override-input args to keep them in sync.
# Only checks root-level inputs to avoid transitive deps and deduplicated nodes (_2, _3).
def follows-overrides [nodes: record, input_name: string, repo: string, tag: string]: nothing -> list<string> {
    $nodes.root.inputs
    | items { |logical_name, node_name|
        if $logical_name == $input_name { return [] }
        let node = $nodes | get $node_name
        if ("inputs" not-in $node) { return [] }
        $node.inputs
        | items { |sub_name, sub_ref|
            if $sub_name != $input_name { return null }
            # follows refs are lists in the lock file (e.g. ["hyprland"]), direct locks are strings
            if ($sub_ref | describe | str starts-with "list") {
                print $"(ansi green_bold)info:(ansi reset) found dependency: ($logical_name)/($input_name)"
                ["--override-input" $"($logical_name)/($input_name)" $"github:($repo)/($tag)"]
            }
        }
        | compact
        | flatten
    }
    | flatten
}

def update-to-latest-release [input_name: string] {
    let lock = open --raw flake.lock | from json
    let original = $lock.nodes | get $input_name | get original

    if $original.type != "github" {
        error make { msg: $"Input '($input_name)' is not a GitHub input" }
    }

    let repo = $"($original.owner)/($original.repo)"

    print $"(ansi blue_bold)info:(ansi reset) fetching latest release tag for ($repo)..."

    let latest_tag = (
        http get $"https://api.github.com/repos/($repo)/releases/latest"
        | get tag_name
    )

    if $latest_tag == null or $latest_tag == "" {
        error make { msg: $"Could not find a latest release for ($repo)" }
    }

    print $"(ansi blue_bold)info:(ansi reset) updating ($input_name) to ($latest_tag)..."
    let extra = follows-overrides $lock.nodes $input_name $repo $latest_tag

    nix flake lock --override-input $input_name $"github:($repo)/($latest_tag)" ...$extra
}

def main [
  ...inputs: string,
  --dir (-d): directory, # override the flake directory (defaults to $NH_FLAKE)
  --all (-a)             # run nix flake update before pinning release-locked inputs
] {
    if $dir == null and "NH_FLAKE" in $env {
        cd $env.NH_FLAKE
    } else if $dir != null {
        cd $dir
    }

    if $all {
        print $"(ansi blue_bold)info:(ansi reset) updating flake inputs..."
        nix flake update
    }

    for input in $inputs {
        update-to-latest-release $input
    }
}
