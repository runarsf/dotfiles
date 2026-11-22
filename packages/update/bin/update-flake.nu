#!/usr/bin/env nu

def info [msg: string] { print $"(ansi blue_bold)info:(ansi reset) ($msg)" }
def warn [msg: string] { print $"(ansi yellow_bold)warn:(ansi reset) ($msg)" }
def fail [msg: string] { print $"(ansi red_bold)error:(ansi reset) ($msg)" }

# Only github.com gets the token; it must never reach another host.
def forge-get [url: string]: nothing -> any {
    if ($url | str starts-with "https://api.github.com/") and ("GITHUB_TOKEN" in $env) {
        http get --headers { Authorization: $"Bearer ($env.GITHUB_TOKEN)" } $url
    } else {
        http get $url
    }
}

def forge-of [original: record]: nothing -> record {
    match $original.type {
        "github" => { provider: "github", repo: $"($original.owner)/($original.repo)", host: "github.com" }
        "gitlab" => {
            let host = if "host" in $original { $original.host } else { "gitlab.com" }
            { provider: "gitlab", repo: $"($original.owner)/($original.repo)", host: $host }
        }
        # A plain git input is taken to be Forgejo/Gitea, which covers Codeberg.
        "git" => {
            let parts = $original.url
                | str replace --regex '^(?:git\+)?https://' ''
                | str replace --regex '\?.*$' ''
                | split row "/"
            { provider: "forgejo", repo: ($parts | skip 1 | str join "/"), host: ($parts | first) }
        }
        _ => { error make { msg: $"unsupported input type: ($original.type)" } }
    }
}

# All three answer with `tag_name` for a release and `name` for a tag.
def forge-endpoints [forge: record]: nothing -> record {
    match $forge.provider {
        "github" => {
            let base = $"https://api.github.com/repos/($forge.repo)"
            { release: $"($base)/releases/latest", tags: $"($base)/tags?per_page=1" }
        }
        "gitlab" => {
            let base = $"https://($forge.host)/api/v4/projects/($forge.repo | str replace '/' '%2F')"
            {
                release: $"($base)/releases/permalink/latest"
                tags: $"($base)/repository/tags?order_by=updated&sort=desc&per_page=1"
            }
        }
        "forgejo" => {
            let base = $"https://($forge.host)/api/v1/repos/($forge.repo)"
            { release: $"($base)/releases/latest", tags: $"($base)/tags?limit=1" }
        }
    }
}

def latest-tag [forge: record]: nothing -> string {
    let api = forge-endpoints $forge
    try {
        try {
            forge-get $api.release | get tag_name
        } catch {
            let tags = forge-get $api.tags
            if ($tags | is-empty) { error make { msg: $"($forge.repo) has no releases or tags" } }
            $tags | first | get name
        }
    } catch { |err|
        if ($err.msg | str contains "403") {
            error make { msg: $"rate limit hit for ($forge.repo) — wait or set GITHUB_TOKEN in the environment" }
        }
        error make { msg: $"could not find a release or tag for ($forge.repo): ($err.msg)" }
    }
}

def flake-url [forge: record, tag: string]: nothing -> string {
    match $forge.provider {
        "github" => $"github:($forge.repo)/($tag)"
        "gitlab" if $forge.host == "gitlab.com" => $"gitlab:($forge.repo)/($tag)"
        "gitlab" => $"gitlab:($forge.repo)/($tag)?host=($forge.host)"
        "forgejo" => $"git+https://($forge.host)/($forge.repo)?ref=refs/tags/($tag)"
    }
}

def base-url [original: record]: nothing -> string {
    match $original.type {
        "github" => $"github:($original.owner)/($original.repo)"
        "gitlab" => $"gitlab:($original.owner)/($original.repo)"
        "git" => ($original.url | str replace --regex '^git\+' '' | str replace --regex '\?.*$' '')
        _ => ""
    }
}

def update-inputs [names: list<string>, token_args: list<string>] {
    if ($names | is-empty) {
        info "updating all flake inputs..."
    } else {
        info $"updating inputs: ($names | str join ', ')..."
    }
    ^nix flake update ...$token_args ...$names
}

# nix-update evaluates the store path `nix flake metadata` reports, which nix
# doesn't realise on its own for a git tree. Prefetch puts the tree there.
def prefetch-flake [] {
    do { ^nix flake prefetch --json . } | complete | ignore
}

# `passthru.autoUpdate = false` pins a package. tryEval keeps one that fails to
# evaluate from hiding the rest.
def updatable-packages []: nothing -> list<string> {
    let system = ^nix eval --impure --raw --expr "builtins.currentSystem"
    let filter = '
      ps: builtins.filter (name:
        let probe = builtins.tryEval (
          let p = ps.${name}; in
            (p.type or "") == "derivation"
            && (p.autoUpdate or true)
            && p ? src
            && (p.version or "") != ""
        );
        in probe.success && probe.value
      ) (builtins.attrNames ps)
    '
    ^nix eval --json --apply $filter $".#packages.($system)" | from json
}

def run-nix-update [pkg: string, ...args: string]: nothing -> record {
    do { ^nix-update --flake ...$args $pkg } | complete
}

def bump-package [pkg: string] {
    prefetch-flake
    let result = run-nix-update $pkg
    # Anything else means nix-update bailed before touching the file.
    let status = $result.stdout | lines | where { |line| $line =~ '^(Update|Not updating) ' }
    if ($status | is-empty) {
        warn $"skipped ($pkg) — run `nix-update --flake ($pkg)` to see why"
        return
    }

    let line = $status | first
    print $"  ($line)"
    if not ($line | str starts-with "Update ") { return }

    # Rewriting the version changed the tree, so the hash needs a second pass
    # against the new store path.
    prefetch-flake
    if (run-nix-update $pkg "--version=skip" | get exit_code) != 0 {
        fail $"($pkg): version bumped but the hash did not update"
    }
}

def update-packages [] {
    let names = updatable-packages
    if ($names | is-empty) {
        warn "no package carries its own version and source"
        return
    }
    info $"updating packages: ($names | str join ', ')..."
    for pkg in $names { bump-package $pkg }
}

def update-fetchers [] {
    info "updating git fetchers..."
    # nix-update-git can't parse pipe operators, so only hand it files that hold
    # a fetcher it knows about.
    let files = do {
        ^rg --files-with-matches --glob '*.nix' 'fetch(git|Git|patch|Tarball|From[A-Z])'
    }
    | complete
    | get stdout
    | lines
    | where { |file| $file | is-not-empty }

    if ($files | is-not-empty) {
        ^nix-update-git --update --rules all -- ...$files
    }
}

# Root level only, to skip transitive and deduplicated nodes (_2, _3).
def follows-overrides [nodes: record, input: string, url: string]: nothing -> list<string> {
    $nodes.root.inputs
    | items { |name, node_name|
        let node = $nodes | get $node_name
        if $name == $input or ("inputs" not-in $node) or ($input not-in $node.inputs) {
            return []
        }
        # A follows ref is a list in the lock file (e.g. ["hyprland"]); a direct
        # lock is a string.
        if (($node.inputs | get $input | describe) | str starts-with "list") {
            info $"found dependency: ($name)/($input)"
            ["--override-input" $"($name)/($input)" $url]
        } else {
            []
        }
    }
    | flatten
}

# nix skips re-locking an input whose commit is unchanged, so a rewritten url
# only takes effect when the input is named on `nix flake update`; an input whose
# url isn't written literally in flake.nix needs an override instead.
def pin-input [nodes: record, input: string]: nothing -> record {
    let original = $nodes | get $input | get original
    let forge = forge-of $original
    info $"fetching latest release tag for ($forge.provider):($forge.repo)..."
    let tag = latest-tag $forge
    let url = flake-url $forge $tag
    info $"pinning ($input) to ($tag)..."

    let base = base-url $original
    let flake = open --raw flake.nix
    if $base == "" or not ($flake | str contains $"\"($base)") {
        warn $"no URL for ($input) in flake.nix, falling back to --override-input"
        let overrides = ["--override-input" $input $url]
            ++ (follows-overrides $nodes $input $url)
        return { overrides: $overrides, relock: [], failed: [] }
    }

    $flake
    | str replace --regex $"\"($base)[^\"]*\"" $"\"($url)\""
    | save --force flake.nix
    { overrides: [], relock: [$input], failed: [] }
}

def pin-releases [inputs: list<string>, token_args: list<string>] {
    let nodes = open --raw flake.lock | from json | get nodes
    let pins = $inputs | each { |input|
        try {
            pin-input $nodes $input
        } catch { |err|
            fail $"($input): ($err.msg)"
            { overrides: [], relock: [], failed: [$input] }
        }
    }
    let overrides = $pins | get overrides | flatten
    let relock = $pins | get relock | flatten
    let failed = $pins | get failed | flatten

    # Re-lock what was pinned even if another input failed, or flake.nix and
    # flake.lock disagree. A bare `nix flake update` would update every input.
    if ($overrides | is-not-empty) or ($relock | is-not-empty) {
        ^nix flake update ...$token_args ...$overrides ...$relock
    }
    if ($failed | is-not-empty) {
        error make --unspanned { msg: $"could not pin: ($failed | str join ', ')" }
    }
}

# Update flake inputs, packages and git fetchers.
#
# Set GITHUB_TOKEN to avoid GitHub's rate limit. Any token works, no scopes
# needed; it is passed on to nix as --extra-access-tokens.
def main [
    ...inputs: string,           # specific inputs to update normally
    --dir (-d): string,          # override the flake directory
    --all (-a),                  # update all regular inputs (implied when nothing is passed)
    --release (-r): string = "", # comma-separated inputs to pin to their latest release
    --packages (-p),             # bump every package's version and source hash
    --fetchgit (-f),             # update git fetchers
] {
    if $dir != null { cd $dir }

    let release_inputs = $release | split row "," | where { |name| $name | is-not-empty }
    let regular_inputs = $inputs | where { |name| $name not-in $release_inputs }
    let token_args = if "GITHUB_TOKEN" in $env {
        ["--extra-access-tokens" $"github.com=($env.GITHUB_TOKEN)"]
    } else {
        []
    }

    if ($regular_inputs | is-not-empty) or $all or ($release_inputs | is-empty) {
        update-inputs $regular_inputs $token_args
    }
    # Packages before fetchers: nix-update also refreshes vendorHash/cargoHash,
    # which nix-update-git leaves alone, so a bumped source would strand those.
    if $packages { update-packages }
    if $fetchgit { update-fetchers }
    if ($release_inputs | is-not-empty) { pin-releases $release_inputs $token_args }
}
