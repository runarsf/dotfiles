#!/usr/bin/env nu

# tip — Typst Install Package
# Manage locally-installed Typst packages from GitHub and Forgejo repositories.

# Returns the platform-appropriate Typst data directory.
def typst-data-dir []: nothing -> string {
    let base = match $nu.os-info.name {
        "macos"   => ($env.HOME | path join "Library" "Application Support"),
        "windows" => $env.APPDATA,
        _         => {
            if "XDG_DATA_HOME" in $env {
                $env.XDG_DATA_HOME
            } else {
                $env.HOME | path join ".local" "share"
            }
        }
    }
    $base | path join "typst"
}

# Parse a repo spec into a { provider, host, owner, name } record.
# Accepts 'owner/repo' (resolves to GitHub) or a full https:// URL.
def parse-repo [spec: string]: nothing -> record {
    if ($spec | str contains "://") {
        let clean = $spec
            | str replace --regex '^https://' ''
            | str replace --regex '/+$' ''
        let parts = $clean | split row "/"
        if ($parts | length) < 3 {
            error make { msg: $"Cannot parse repo URL: ($spec)" }
        }
        let provider = if ($parts.0) == "github.com" { "github" } else { "forgejo" }
        { provider: $provider, host: $parts.0, owner: $parts.1, name: $parts.2 }
    } else if ($spec | str contains "/") {
        let parts = $spec | split row "/"
        { provider: "github", host: "github.com", owner: $parts.0, name: $parts.1 }
    } else {
        error make { msg: $"Invalid repo spec '($spec)'. Use 'owner/repo' or a full https:// URL." }
    }
}

# Fetch the latest release tag. Returns null if no releases exist.
def fetch-latest-release [info: record]: nothing -> string {
    match $info.provider {
        "github" => {
            try {
                http get $"https://api.github.com/repos/($info.owner)/($info.name)/releases/latest"
                | get tag_name
            } catch {
                null
            }
        }
        "forgejo" => {
            let releases = try {
                http get $"https://($info.host)/api/v1/repos/($info.owner)/($info.name)/releases?limit=1"
            } catch {
                []
            }
            if ($releases | is-empty) { null } else { $releases | first | get tag_name }
        }
    }
}

# Fetch the default branch name for a repository.
def fetch-default-branch [info: record]: nothing -> string {
    match $info.provider {
        "github" => {
            http get $"https://api.github.com/repos/($info.owner)/($info.name)"
            | get default_branch
        }
        "forgejo" => {
            http get $"https://($info.host)/api/v1/repos/($info.owner)/($info.name)"
            | get default_branch
        }
    }
}

# Construct the archive URL for a git tag.
def tag-archive-url [info: record, tag: string]: nothing -> string {
    match $info.provider {
        "github"  => $"https://github.com/($info.owner)/($info.name)/archive/refs/tags/($tag).tar.gz"
        "forgejo" => $"https://($info.host)/($info.owner)/($info.name)/archive/($tag).tar.gz"
    }
}

# Construct the archive URL for a branch.
def branch-archive-url [info: record, branch: string]: nothing -> string {
    match $info.provider {
        "github"  => $"https://github.com/($info.owner)/($info.name)/archive/refs/heads/($branch).tar.gz"
        "forgejo" => $"https://($info.host)/($info.owner)/($info.name)/archive/($branch).tar.gz"
    }
}

# Download an archive from a URL, extract it into a temp dir, and return
# the path of the extracted top-level directory.
def download-and-extract [url: string]: nothing -> string {
    let tmp = mktemp -d
    let archive = $tmp | path join "pkg.tar.gz"

    print $"(ansi blue_bold)tip:(ansi reset) downloading ($url)..."
    try {
        http get $url | save --force $archive
    } catch { |e|
        rm --recursive --force $tmp
        error make { msg: $"Download failed: ($e.msg)" }
    }

    print $"(ansi blue_bold)tip:(ansi reset) extracting..."
    try {
        ^tar -xzf $archive -C $tmp
    } catch { |e|
        rm --recursive --force $tmp
        error make { msg: $"Extraction failed: ($e.msg)" }
    }
    rm --force $archive

    let dirs = ls $tmp | where type == dir | get name
    if ($dirs | is-empty) {
        rm --recursive --force $tmp
        error make { msg: "Archive contained no top-level directory." }
    }
    $dirs | first
}

# Read typst.toml from an extracted directory, returning { name, version }.
def read-manifest [extracted: string]: nothing -> record {
    let toml_path = $extracted | path join "typst.toml"
    if not ($toml_path | path exists) {
        error make { msg: "No typst.toml found at the repository root. Is this a Typst package?" }
    }
    let manifest = open $toml_path
    { name: $manifest.package.name, version: $manifest.package.version }
}

# Install a Typst package from a GitHub or Forgejo/Codeberg repository.
#
# Resolves in order: explicit version tag → latest release → default branch.
# The package name and install version are always read from typst.toml.
#
# Examples:
#   tip install typst-community/charged-ieee
#   tip install typst-community/charged-ieee v0.1.2
#   tip install https://codeberg.org/someuser/mypkg
def "main install" [
    repo: string,                       # 'owner/repo' (GitHub) or full https:// URL
    version?: string,                   # Git tag to install (default: latest release)
    --namespace (-n): string = "local", # Typst package namespace (default: local)
    --force (-f),                       # Overwrite an existing installation
] {
    let info = parse-repo $repo
    print $"(ansi blue_bold)tip:(ansi reset) resolving ($info.owner)/($info.name) \(($info.provider) @ ($info.host)\)..."

    let source = if $version != null {
        { url: (tag-archive-url $info $version), label: $version }
    } else {
        print $"(ansi blue_bold)tip:(ansi reset) checking for releases..."
        let release_tag = fetch-latest-release $info
        if $release_tag != null {
            print $"(ansi blue_bold)tip:(ansi reset) latest release: (ansi green)($release_tag)(ansi reset)"
            { url: (tag-archive-url $info $release_tag), label: $release_tag }
        } else {
            print $"(ansi yellow_bold)tip:(ansi reset) no releases found, using default branch..."
            let branch = fetch-default-branch $info
            { url: (branch-archive-url $info $branch), label: $branch }
        }
    }

    let extracted = download-and-extract $source.url
    let tmp = $extracted | path dirname

    let pkg = try { read-manifest $extracted } catch { |e|
        rm --recursive --force $tmp
        error make { msg: $e.msg }
    }

    let install_dir = (typst-data-dir) | path join "packages" $namespace $pkg.name $pkg.version

    if ($install_dir | path exists) {
        if $force {
            print $"(ansi yellow_bold)tip:(ansi reset) overwriting existing installation at ($install_dir)..."
            rm --recursive --force $install_dir
        } else {
            rm --recursive --force $tmp
            error make { msg: $"($pkg.name) ($pkg.version) is already installed. Use --force to overwrite." }
        }
    }

    mkdir ($install_dir | path dirname)
    ^cp -r $extracted $install_dir
    rm --recursive --force $tmp

    print $"(ansi green_bold)tip:(ansi reset) installed (ansi cyan)($pkg.name)(ansi reset) (ansi cyan)($pkg.version)(ansi reset) → ($install_dir)"
}

# Uninstall a locally-installed Typst package.
# Removes all versions unless a specific version is given.
#
# Examples:
#   tip uninstall charged-ieee
#   tip uninstall charged-ieee v0.1.2
def "main uninstall" [
    name: string,                       # Package name (as in typst.toml)
    version?: string,                   # Version to remove (default: all versions)
    --namespace (-n): string = "local", # Typst package namespace (default: local)
] {
    let pkg_base = (typst-data-dir) | path join "packages" $namespace $name

    if not ($pkg_base | path exists) {
        error make { msg: $"Package '($name)' is not installed." }
    }

    if $version != null {
        # Strip leading 'v' — Typst version dirs don't use it (e.g. 0.1.2, not v0.1.2)
        let ver = $version | str replace --regex '^v' ''
        let install_dir = $pkg_base | path join $ver
        if not ($install_dir | path exists) {
            error make { msg: $"($name) ($ver) is not installed." }
        }
        rm --recursive --force $install_dir
        print $"(ansi green_bold)tip:(ansi reset) uninstalled (ansi cyan)($name)(ansi reset) (ansi cyan)($ver)(ansi reset)"
        # Clean up the now-empty package directory if applicable
        if (ls $pkg_base | is-empty) {
            rm --recursive --force $pkg_base
        }
    } else {
        let versions = ls $pkg_base | get name | path basename
        rm --recursive --force $pkg_base
        print $"(ansi green_bold)tip:(ansi reset) uninstalled (ansi cyan)($name)(ansi reset) \(($versions | length) version(s): ($versions | str join ', ')\)"
    }
}

# List locally-installed Typst packages.
# Without arguments, lists all packages across all namespaces.
# Pass a name to show only versions of that package.
#
# Examples:
#   tip list
#   tip list charged-ieee
#   tip list --namespace preview
def "main list" [
    name?: string,                      # Filter by package name
    --namespace (-n): string,           # Filter by namespace (default: all)
] {
    let packages_dir = (typst-data-dir) | path join "packages"

    if not ($packages_dir | path exists) {
        print "No packages installed."
        return
    }

    # Collect { namespace, name, version } for every installed version
    let rows = ls $packages_dir
        | where type == dir
        | get name
        | each { |ns_dir|
            let ns = $ns_dir | path basename
            if $namespace != null and $ns != $namespace { return [] }
            ls $ns_dir
            | where type == dir
            | get name
            | each { |pkg_dir|
                let pkg_name = $pkg_dir | path basename
                if $name != null and $pkg_name != $name { return [] }
                ls $pkg_dir
                | where type == dir
                | get name
                | each { |ver_dir|
                    { namespace: $ns, name: $pkg_name, version: ($ver_dir | path basename) }
                }
            }
            | flatten
        }
        | flatten

    if ($rows | is-empty) {
        print "No packages installed."
        return
    }

    $rows | sort-by namespace name version
}

def main [] {
    print $"(ansi blue_bold)tip:(ansi reset) for help, run (ansi yellow)tip --help(ansi reset)"
}
