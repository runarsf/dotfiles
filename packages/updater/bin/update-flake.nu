#!/usr/bin/env nu

# Scans direct top-level inputs (via root node) for any that follow `input_name`,
# returning explicit --override-input args to keep them in sync.
# Only checks root-level inputs to avoid transitive deps and deduplicated nodes (_2, _3).
def follows-overrides [nodes: record, input_name: string, nix_url: string]: nothing -> list<string> {
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
                ["--override-input" $"($logical_name)/($input_name)" $nix_url]
            }
        }
        | compact
        | flatten
    }
    | flatten
}

# Extracts provider, repo path, and host from a lock node's original field.
# Returns: { provider: string, repo: string, host: string }
def get-provider-info [original: record]: nothing -> record {
    match $original.type {
        "github" => {
            { provider: "github", repo: $"($original.owner)/($original.repo)", host: "github.com" }
        }
        "gitlab" => {
            let host = if "host" in $original { $original.host } else { "gitlab.com" }
            { provider: "gitlab", repo: $"($original.owner)/($original.repo)", host: $host }
        }
        "git" => {
            # Parse host and repo from the URL (e.g. git+https://codeberg.org/LGFae/awww)
            let clean = $original.url
                | str replace --regex '^(?:git\+)?https://' ''
                | str replace --regex '\?.*$' ''
            let parts = $clean | split row "/"
            let host = $parts | first
            let repo = $parts | skip 1 | str join "/"
            # Forgejo/Gitea API (covers Codeberg and other instances)
            { provider: "forgejo", repo: $repo, host: $host }
        }
        _ => {
            error make { msg: $"Unsupported input type: ($original.type)" }
        }
    }
}

# Fetches the latest release tag from the appropriate forge API.
def fetch-latest-tag [info: record]: nothing -> string {
    match $info.provider {
        "github" => {
            http get $"https://api.github.com/repos/($info.repo)/releases/latest"
            | get tag_name
        }
        "gitlab" => {
            let encoded = $info.repo | str replace "/" "%2F"
            http get $"https://($info.host)/api/v4/projects/($encoded)/releases/permalink/latest"
            | get tag_name
        }
        "forgejo" => {
            http get $"https://($info.host)/api/v1/repos/($info.repo)/releases/latest"
            | get tag_name
        }
    }
}

# Returns the nix flake URL for a given provider/repo/tag.
def make-nix-url [info: record, tag: string]: nothing -> string {
    match $info.provider {
        "github" => { $"github:($info.repo)/($tag)" }
        "gitlab" => {
            if $info.host == "gitlab.com" {
                $"gitlab:($info.repo)/($tag)"
            } else {
                $"gitlab:($info.repo)/($tag)?host=($info.host)"
            }
        }
        "forgejo" => { $"git+https://($info.host)/($info.repo)?ref=refs/tags/($tag)" }
    }
}

def update-to-latest-release [input_name: string] {
    let lock = open --raw flake.lock | from json
    let original = $lock.nodes | get $input_name | get original
    let info = get-provider-info $original

    print $"(ansi blue_bold)info:(ansi reset) fetching latest release tag for ($info.repo) \(($info.provider)\)..."

    let latest_tag = fetch-latest-tag $info

    if $latest_tag == null or $latest_tag == "" {
        error make { msg: $"Could not find a latest release for ($info.repo)" }
    }

    let nix_url = make-nix-url $info $latest_tag
    print $"(ansi blue_bold)info:(ansi reset) updating ($input_name) to ($latest_tag)..."

    let extra = follows-overrides $lock.nodes $input_name $nix_url
    nix flake lock --override-input $input_name $nix_url ...$extra
}

def main [
  ...release_inputs: string,
  --dir (-d): string,  # override the flake directory (defaults to $NH_FLAKE)
  --inputs,               # run nix flake update before pinning release-locked inputs
  --fetchgit              # update git fetchers
] {
    if $dir != null {
        cd $dir
    }

    if $inputs {
        print $"(ansi blue_bold)info:(ansi reset) updating flake inputs..."
        nix flake update
    }

    if $fetchgit {
        print $"(ansi blue_bold)info:(ansi reset) updating fetchgit..."
        let nix_files = ^fd --extension nix --type file | lines
        ^update-nix-fetchgit --verbose ...$nix_files out+err>| lines | where not ($it =~ '^Made.*updates$') | each { print $in }
    }

    for input in $release_inputs {
        update-to-latest-release $input
    }
}
