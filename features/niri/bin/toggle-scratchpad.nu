def main [] {
  let windows = (niri msg -j windows | from json | where app_id == "scratchpad")

  if ($windows | is-empty) {
    niri msg action spawn -- wezterm start --class scratchpad
  } else {
    let scratchpad = ($windows | first)
    let focused_workspace = (niri msg -j workspaces | from json | where is_focused | first)

    if $scratchpad.workspace_id == $focused_workspace.id {
      ^niri msg action move-window-to-workspace --window-id $scratchpad.id --focus false scratch
    } else {
      ^niri msg action move-window-to-workspace --window-id $scratchpad.id $focused_workspace.idx
      ^niri msg action focus-window --id $scratchpad.id
    }
  }
}
