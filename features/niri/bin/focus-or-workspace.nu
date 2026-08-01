def main [direction: string] {
  let focus_workspace = $env.NIRI_FOCUS_WORKSPACE
  let before = try { niri msg -j focused-window | from json | get id }
  if $before == null {
    ^$focus_workspace $direction
    return
  }
  niri msg action $"focus-window-($direction)"
  let after = (niri msg -j focused-window | from json | get id)
  if $before == $after {
    ^$focus_workspace $direction
  }
}
