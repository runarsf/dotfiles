def main [] {
  let workspaces = ($env.NIRI_NAMED_WORKSPACES | split row " " | filter { |x| not ($x | is-empty) })
  let ws_offset = ($env.NIRI_WS_OFFSET | into int)

  $workspaces | enumerate | each { |entry|
    niri msg action move-workspace-to-index --reference $entry.item ($entry.index + 1)
  }
  niri msg action focus-workspace ($ws_offset + 1)
}
