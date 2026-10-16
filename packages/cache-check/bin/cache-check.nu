#!/usr/bin/env nu

# Checks whether a flake output is substitutable from a given Cachix cache,
# without building or downloading it. Useful for telling whether a
# release-locked input (or any input) actually has a cached build, since a
# fresh commit/tag is no guarantee the project's cache has built it.

def get-system []: nothing -> string {
  ^nix eval --impure --raw --expr "builtins.currentSystem"
}

# Resolves a bare input name (flake.lock), "owner/repo", or a full flake ref
# to a base url (no ref) plus the rev to default to and a short name for
# guessing the cache/attr.
def resolve-base [name: string, dir: string]: nothing -> record {
  if ($name | str contains ":") {
    return { base: $name, rev: null, short: ($name | split row "/" | last) }
  }

  if ($name | str contains "/") {
    let parts = $name | split row "/"
    return { base: $"github:($parts | first)/($parts | get 1)", rev: null, short: ($parts | get 1) }
  }

  let lock_path = $"($dir)/flake.lock"
  if not ($lock_path | path exists) {
    error make { msg: $"($name) is not a flake ref or owner/repo, and ($lock_path) doesn't exist to look it up as an input" }
  }
  let lock = open --raw $lock_path | from json
  if $name not-in $lock.nodes {
    error make { msg: $"($name) is not an input in ($lock_path) — pass owner/repo or a full flake ref instead" }
  }
  let node = $lock.nodes | get $name
  let original = $node.original
  if $original.type != "github" {
    error make { msg: $"($name) is a ($original.type) input — pass its full flake ref explicitly" }
  }
  { base: $"github:($original.owner)/($original.repo)", rev: $node.locked.rev, short: $original.repo }
}

# Prefers `default`, then the repo's short name, else lists what's available.
def resolve-attr [flake_ref: string, system: string, short: string]: nothing -> string {
  let names = (^nix eval $"($flake_ref)#packages.($system)" --apply "builtins.attrNames" --json | from json)
  if "default" in $names {
    "default"
  } else if $short in $names {
    $short
  } else {
    error make { msg: $"couldn't guess a package attr for ($flake_ref); pass --attr, pick one of: ($names | str join ', ')" }
  }
}

# Checks whether a flake input is cached, without building or downloading it.
@example "check what a flake input is currently locked to" { cache-check hyprland }
@example "check a different rev/tag than what's locked" { cache-check hyprland --ref v0.56.2 }
@example "owner/repo, no flake.lock lookup needed" { cache-check hyprwm/Hyprland }
@example "a full flake ref" { cache-check "github:zed-industries/zed" }
@example "override the guessed attr and cache name" { cache-check hyprland-plugins hyprland --attr hyprbars }
def main [
  input: string,      # flake input name (from flake.lock), "owner/repo", or a full flake ref
  cache?: string,      # cachix cache name; defaults to the repo's short name
  --dir (-d): string,  # flake directory to look up `input` in (default: $NH_FLAKE or .)
  --ref: string,       # rev/tag to check instead of the locked (or default-branch) one
  --attr: string,      # package attribute; auto-detected if omitted
  --system: string,    # defaults to the current system
] {
  let dir = if $dir != null { $dir } else if "NH_FLAKE" in $env { $env.NH_FLAKE } else { "." }
  let resolved = resolve-base $input $dir
  let rev = if $ref != null { $ref } else { $resolved.rev }
  let base = if $rev != null { $"($resolved.base)/($rev)" } else { $resolved.base }
  let system = if $system != null { $system } else { get-system }
  let attr = if $attr != null { $attr } else { resolve-attr $base $system $resolved.short }
  let cache_name = if $cache != null { $cache } else { $resolved.short | str lowercase }

  let installable = $"($base)#packages.($system).($attr)"

  print $"(ansi blue_bold)info:(ansi reset) evaluating ($installable)..."
  let out_path = (do { ^nix eval --raw $"($installable).outPath" } | complete)
  if $out_path.exit_code != 0 {
    error make { msg: $"failed to evaluate ($installable): ($out_path.stderr | str trim)" }
  }
  let path = ($out_path.stdout | str trim)
  let hash = ($path | path basename | split row "-" | first)
  let url = $"https://($cache_name).cachix.org/($hash).narinfo"

  let response = (http get --full --allow-errors $url)
  let code = $response.status

  print $"(ansi cyan_bold)store path:(ansi reset) ($path)"
  print $"(ansi cyan_bold)narinfo:(ansi reset)    ($url)"

  if $code == 200 {
    print $"(ansi green_bold)cached(ansi reset) on ($cache_name).cachix.org"
  } else if $code == 404 {
    print $"(ansi red_bold)not cached(ansi reset) on ($cache_name).cachix.org"
  } else {
    print $"(ansi yellow_bold)unexpected status ($code)(ansi reset) from ($cache_name).cachix.org"
  }
}
