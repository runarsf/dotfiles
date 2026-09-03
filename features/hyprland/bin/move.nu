def main [action: string, dir: string] {
  let window = hyprctl -j activewindow | from json

  # No active window (e.g. empty workspace): the group/monocle checks below
  # need an active window to inspect, so skip straight to the plain
  # focus/monitor dispatch. There's nothing to "move" without a window.
  if ($window | columns | is-empty) {
    if $action == "focus" {
      hyprctl --batch $"dispatch hl.dsp.focus\({ direction = \"($dir)\" }); dispatch hl.dsp.cursor.move_to_corner\({ corner = 2 })"
    }
    return
  }

  let workspaces = hyprctl -j workspaces | from json
  let workspace = $workspaces | where id == $window.workspace.id | first

  let monocle: bool = $workspace | get hasfullscreen
  let group: record = {
    grouped: ($window | get grouped | is-not-empty)
    first: (try {
      ($window | get grouped | first) == ($window | get address)
    } catch {
      false
    })
    last: (try {
      ($window | get grouped | last) == ($window | get address)
    } catch {
      false
    })
  }

  if ($group.grouped and $dir == "left" and not $group.first) {
    match $action {
      "focus" => { hyprctl dispatch "hl.dsp.group.prev()" },
      "move"  => { hyprctl dispatch "hl.dsp.group.move_window({ forward = false })" }
    }
  } else if ($group.grouped and $dir == "right" and not $group.last) {
    match $action {
      "focus" => { hyprctl dispatch "hl.dsp.group.next()" },
      "move"  => { hyprctl dispatch "hl.dsp.group.move_window({ forward = true })" }
    }
  } else if ($monocle and $dir == "up") {
    match $action {
      "focus" => { hyprctl dispatch "hl.dsp.window.cycle_next({ next = false })" },
      "move"  => { hyprctl dispatch "hl.dsp.window.swap({ next = false })" }
    }
  } else if ($monocle and $dir == "down") {
    match $action {
      "focus" => { hyprctl dispatch "hl.dsp.window.cycle_next({ next = true })" },
      "move"  => { hyprctl dispatch "hl.dsp.window.swap({ next = true })" }
    }
  } else {
    match $action {
      "focus" => { hyprctl --batch $"dispatch hl.dsp.focus\({ direction = \"($dir)\" }); dispatch hl.dsp.cursor.move_to_corner\({ corner = 2 })" },
      "move"  => { hyprctl dispatch $"hl.dsp.window.move\({ direction = \"($dir)\" })" }
    }
  }
}
