#!/usr/bin/env nu

# Helpers for the one-shot steps declared in modules.migrations.entries.
# Every subcommand prints what it would do instead of doing it when
# home-manager runs with --dry-run, which it passes down as DRY_RUN.

def dry-run-env []: nothing -> bool {
  $env.DRY_RUN? != null
}

# Unlike `path exists` this is true for a symlink whose target is gone,
# which is what a nix-managed path looks like between generations.
def present [p: path]: nothing -> bool {
  ($p | path type) != null
}

# Copies everything under src that dst doesn't already have, so nix-managed
# paths and whatever activation wrote earlier always win.
@example "bring an old browser profile into the nix-managed one" { hm-migrate merge ~/.zen/runar ~/.config/zen/default --mode 700 }
def "main merge" [
  src: path, # directory to take files from
  dst: path, # directory to fill in
  --mode: string = "", # chmod for dst once the copy is done
  --dry-run, # print what would happen and change nothing
]: nothing -> nothing {
  let dry = $dry_run or (dry-run-env)
  let from = $src | path expand
  let into = $dst | path expand

  if not (present $from) {
    error make { msg: $"merge: ($from) does not exist" }
  }

  if $from == $into {
    print $"migrate: ($from) is already the target, nothing to copy"
    return
  }

  if not (present $into) {
    if $dry { print $"migrate: would create ($into)" } else { mkdir $into }
  }

  let files = ^find $from -mindepth 1 -not -type d -printf "%P\n" | lines
  let missing = $files | where {|f| not (present ($into | path join $f)) }
  let kept = ($files | length) - ($missing | length)

  if ($missing | is-empty) {
    print $"migrate: ($into) already has every file from ($from)"
  } else if $dry {
    print $"migrate: would copy ($missing | length) files from ($from) to ($into), keeping ($kept) already there"
  } else {
    cd $from
    ^cp -a --parents ...$missing $into
    print $"migrate: copied ($missing | length) files from ($from) to ($into), kept ($kept) already there"
  }

  if $mode != "" {
    if $dry { print $"migrate: would chmod ($mode) ($into)" } else { ^chmod $mode $into }
  }
}

# Renames a path out of the way, timestamped, so the move can be undone by
# hand. Deleting is left to whoever reads the message.
@example "retire a profile root the browser would otherwise keep using" { hm-migrate aside ~/.zen }
def "main aside" [
  target: path, # path to rename
  --dry-run, # print what would happen and change nothing
]: nothing -> nothing {
  let dry = $dry_run or (dry-run-env)
  let path = $target | path expand --no-symlink

  if not (present $path) {
    print $"migrate: ($path) is already gone"
    return
  }

  let stamp = date now | format date "%Y%m%d-%H%M%S"
  let dest = $"($path).bak-($stamp)"

  if $dry {
    print $"migrate: would move ($path) to ($dest)"
  } else {
    ^mv $path $dest
    print $"migrate: moved ($path) to ($dest)"
  }
}

# Prints the first matching path something still holds open, nothing when
# they are all free. Used to leave a step for the next switch instead of
# touching files the app is working on.
@example "is the browser still using either profile?" { hm-migrate busy ~/.zen/*/.parentlock ~/.config/zen/default/.parentlock }
def "main busy" [
  ...patterns: string, # glob patterns to check
]: nothing -> nothing {
  for pattern in $patterns {
    for file in (glob $pattern) {
      let holders = ^lsof -t -- $file | complete
      if $holders.exit_code == 0 and ($holders.stdout | str trim | is-not-empty) {
        print $file
        return
      }
    }
  }
}

def main []: nothing -> nothing {
  help main
}
