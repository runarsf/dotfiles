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

# nix-update evaluates the store path `nix flake metadata` reports, which nix
# doesn't realise on its own for a git tree. Prefetch puts the tree there.
def prefetch-flake []: nothing -> nothing {
    do { ^nix flake prefetch --json . } | complete | ignore
}

# Every package nix-update can bump: a derivation carrying its own version and
# source. `passthru.autoUpdate = false` pins one. tryEval keeps a package that
# fails to evaluate from hiding the rest.
def updatable-packages []: nothing -> list<string> {
    let system = ^nix eval --impure --raw --expr "builtins.currentSystem"
    let filter = 'ps: builtins.filter (n: let r = builtins.tryEval (let p = ps.${n}; in (p.type or "") == "derivation" && (p.autoUpdate or true) && p ? src && (p.version or "") != ""); in r.success && r.value) (builtins.attrNames ps)'
    ^nix eval --json --apply $filter $".#packages.($system)" | from json
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

def github-get [url: string]: nothing -> any {
    if "GITHUB_TOKEN" in $env {
        http get --headers {Authorization: $"Bearer ($env.GITHUB_TOKEN)"} $url
    } else {
        http get $url
    }
}

# Fetches the latest release or tag from the appropriate forge API.
# Falls back to the tags API if no releases exist.
def fetch-latest-tag [info: record]: nothing -> string {
    try {
        match $info.provider {
            "github" => {
                try {
                    github-get $"https://api.github.com/repos/($info.repo)/releases/latest"
                    | get tag_name
                } catch {
                    let tags = github-get $"https://api.github.com/repos/($info.repo)/tags?per_page=1"
                    if ($tags | is-empty) { error make { msg: $"($info.repo) has no releases or tags" } }
                    $tags | first | get name
                }
            }
            "gitlab" => {
                let encoded = $info.repo | str replace "/" "%2F"
                try {
                    http get $"https://($info.host)/api/v4/projects/($encoded)/releases/permalink/latest"
                    | get tag_name
                } catch {
                    http get $"https://($info.host)/api/v4/projects/($encoded)/repository/tags?order_by=updated&sort=desc&per_page=1"
                    | first
                    | get name
                }
            }
            "forgejo" => {
                try {
                    http get $"https://($info.host)/api/v1/repos/($info.repo)/releases/latest"
                    | get tag_name
                } catch {
                    http get $"https://($info.host)/api/v1/repos/($info.repo)/tags?limit=1"
                    | first
                    | get name
                }
            }
        }
    } catch { |err|
        let msg = if ($err.msg | str contains "403") {
            $"rate limit hit for ($info.repo) — wait or set GITHUB_TOKEN in the environment"
        } else {
            $"could not find a release or tag for ($info.repo): ($err.msg)"
        }
        error make { msg: $msg }
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


def main [
  ...inputs: string,                  # specific inputs to update normally
  --dir (-d): string,                 # override the flake directory
  --all (-a),                         # update all regular inputs (implied when nothing is passed)
  --release (-r): string = "",        # comma-separated inputs to pin to their latest release
  --packages (-p),                    # bump every package's version and source hash
  --fetchgit (-f),                    # update git fetchers
] {
    if $dir != null {
        cd $dir
    }

    let release_inputs = $release | split row "," | where { |x| not ($x | is-empty) }
    let regular_inputs = $inputs | where { |x| $x not-in $release_inputs }
    let nix_token_args = if "GITHUB_TOKEN" in $env {
        ["--extra-access-tokens" $"github.com=($env.GITHUB_TOKEN)"]
    } else {
        []
    }

    let do_regular = ($regular_inputs | is-not-empty) or $all or ($release_inputs | is-empty)

    if $do_regular {
        if ($regular_inputs | is-not-empty) {
            print $"(ansi blue_bold)info:(ansi reset) updating inputs: ($regular_inputs | str join ', ')..."
            nix flake update ...$nix_token_args ...$regular_inputs
        } else {
            print $"(ansi blue_bold)info:(ansi reset) updating all flake inputs..."
            nix flake update ...$nix_token_args
        }
    }

    # Before the fetchers: nix-update also refreshes vendorHash/cargoHash, which
    # nix-update-git leaves alone. Bumping the source first would strand those.
    if $packages {
        let package_names = updatable-packages
        if ($package_names | is-empty) {
            print $"(ansi yellow_bold)warn:(ansi reset) no package carries its own version and source"
        } else {
            print $"(ansi blue_bold)info:(ansi reset) updating packages: ($package_names | str join ', ')..."
            for pkg in $package_names {
                prefetch-flake
                let status = do { ^nix-update --flake $pkg }
                    | complete
                    | get stdout
                    | lines
                    | where { |l| ($l | str starts-with "Update ") or ($l | str starts-with "Not updating") }
                if ($status | is-empty) {
                    print $"(ansi yellow_bold)warn:(ansi reset) skipped ($pkg) — run `nix-update --flake ($pkg)` to see why"
                    continue
                }
                print $"  ($status | first)"
                if ($status | first | str starts-with "Update ") {
                    # Rewriting the version changed the tree, so the source hash
                    # needs a second pass against the new store path.
                    prefetch-flake
                    let rehash = do { ^nix-update --flake --version=skip $pkg } | complete
                    if $rehash.exit_code != 0 {
                        print $"(ansi red_bold)error:(ansi reset) ($pkg): version bumped but the hash did not update"
                    }
                }
            }
        }
    }

    if $fetchgit {
        print $"(ansi blue_bold)info:(ansi reset) updating git fetchers..."
        # nix-update-git can't parse pipe operators yet, so only hand it files
        # that hold a fetcher it knows about.
        let found = do {
            ^rg --files-with-matches --glob '*.nix' 'fetch(git|Git|patch|Tarball|From[A-Z])'
        } | complete
        let nix_files = $found.stdout | lines | where { |x| not ($x | is-empty) }
        if ($nix_files | is-not-empty) {
            ^nix-update-git --update --rules all -- ...$nix_files
        }
    }

    if ($release_inputs | is-not-empty) {
        let lock = open --raw flake.lock | from json
        mut override_args: list<string> = []
        mut update_inputs: list<string> = []

        for input in $release_inputs {
            let original = $lock.nodes | get $input | get original
            let info = get-provider-info $original
            print $"(ansi blue_bold)info:(ansi reset) fetching latest release tag for ($info.provider):($info.repo)..."
            let latest_tag = fetch-latest-tag $info
            let nix_url = make-nix-url $info $latest_tag
            print $"(ansi blue_bold)info:(ansi reset) pinning ($input) to ($latest_tag)..."

            # Construct the base URL as it appears in flake.nix (without any tag/ref).
            # nix skips rewriting the lock when the commit is unchanged, so --override-input
            # won't set original.ref in that case. Updating flake.nix directly is reliable.
            let base_url = match $original.type {
                "github" => { $"github:($original.owner)/($original.repo)" }
                "gitlab" => { $"gitlab:($original.owner)/($original.repo)" }
                "git"    => { $original.url | str replace --regex '^git\+' '' | str replace --regex '\?.*$' '' }
                _        => { "" }
            }
            let old_flake = open --raw flake.nix
            if $base_url == "" or not ($old_flake | str contains $"\"($base_url)") {
                print $"(ansi yellow_bold)warn:(ansi reset) could not find URL for ($input) in flake.nix, falling back to --override-input"
                let extra = follows-overrides $lock.nodes $input $nix_url
                $override_args = ($override_args | append (["--override-input" $input $nix_url] ++ $extra))
            } else {
                let new_flake = $old_flake | str replace --regex $"\"($base_url)[^\"]*\"" $"\"($nix_url)\""
                $new_flake | save --force flake.nix
                # bare `nix flake lock` won't re-resolve an already-locked input
                # just because its url text changed, so force it explicitly.
                $update_inputs = ($update_inputs | append $input)
            }
        }

        nix flake update ...$nix_token_args ...$override_args ...$update_inputs
    }
}
