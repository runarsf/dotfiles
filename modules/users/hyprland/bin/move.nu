#! /usr/bin/env cached-nix-shell
#! nix-shell -i nu -p nushell

# TODO Should move to other workspace if you're on an empty one, same for special workspaces and floating windows

def main [action: string, dir: string] {
  # Get active workspace (activeworkspace doesn't include special workspaces)
  let window: table = hyprctl -j activewindow | from json
  let workspaces: table = hyprctl -j workspaces | from json
  let workspace: table = $workspaces | where id == $window.workspace.id | first

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

  if ($group.grouped and $dir == "l" and not $group.first) {
    match $action {
      "focus" => { hyprctl dispatch changegroupactive b },
      "move"  => { hyprctl dispatch movegroupwindow b }
    }
  } else if ($group.grouped and $dir == "r" and not $group.last) {
    match $action {
      "focus" => { hyprctl dispatch changegroupactive f },
      "move"  => { hyprctl dispatch movegroupwindow f }
    }
  } else if ($monocle and $dir == "u") {
    match $action {
      "focus" => { hyprctl dispatch cyclenext prev hist },
      "move"  => { hyprctl dispatch swapnext prev }
    }
  } else if ($monocle and $dir == "d") {
    match $action {
      "focus" => { hyprctl dispatch cyclenext hist },
      "move"  => { hyprctl dispatch swapnext }
    }
  } else {
    match $action {
      "focus" => { hyprctl --batch $"dispatch movefocus ($dir); dispatch movecursortocorner 2" },
      "move"  => { hyprctl dispatch movewindow $dir }
    }
  }
}

