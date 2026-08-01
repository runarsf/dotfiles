def main [direction: string] {
  let excluded = ($env.NIRI_EXCLUDED_WORKSPACES? | default "" | split row " " | filter { |x| not ($x | is-empty) })
  let workspaces = (niri msg -j workspaces | from json | sort-by idx)
  let windows = (niri msg -j windows | from json)
  let occupied_ids = ($windows | get workspace_id | uniq)

  let current = ($workspaces | where is_focused | first)

  let candidates = (if $direction == "down" {
    $workspaces | where idx > $current.idx
  } else {
    $workspaces | where idx < $current.idx | reverse
  } | where {|ws| ($ws.name == null) or (not ($ws.name in $excluded))})

  let named_ids = ($workspaces | where {|ws| ($ws.name != null) and (not ($ws.name in $excluded))} | get id)
  let any_named_occupied = ($named_ids | any {|id| $id in $occupied_ids})

  let filtered = if $any_named_occupied {
    $candidates | where {|ws|
      let is_empty_named = ($ws.name != null) and (not ($ws.id in $occupied_ids))
      not $is_empty_named
    }
  } else {
    $candidates
  }

  let target = if not ($filtered | is-empty) {
    $filtered | first
  } else if not ($candidates | is-empty) {
    $candidates | first
  } else {
    null
  }

  if $target != null {
    niri msg action focus-workspace $target.idx
  }
}
