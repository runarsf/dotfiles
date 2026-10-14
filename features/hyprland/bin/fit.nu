def from-css [] {
  $in | split words | zip [n e s w] | reduce -f {} {|it, acc| $acc | insert $it.1 ($it.0 | into int)}
}

def main [] {
  let window = hyprctl -j activewindow | from json
  let monitors = hyprctl -j monitors | from json
  let workspaces = hyprctl -j workspaces | from json

  if ($window | is-empty) {
    return
  }

  let monitor = $monitors | where id == $window.monitor | first
  let workspace = $workspaces | where id == $monitor.activeWorkspace.id | first

  let window_width = $window.size | first
  let gaps = hyprctl -j getoption general.gaps_out | from json | get css | from-css
  let gaps_in = hyprctl -j getoption general.gaps_in | from json | get css | from-css
  let border_size = hyprctl -j getoption general.border_size | from json | get int

  # a fully-resized column still keeps one inner gap per side that has a neighboring column
  # (dedupe by x since a column can hold multiple stacked windows, and skip floating windows
  # since their position isn't part of the column layout)
  let column_positions = hyprctl -j clients | from json | where {|c| $c.workspace.id == $workspace.id and not $c.floating} | each {|c| $c.at | first} | uniq
  let window_x = $window.at | first
  let has_left = ($column_positions | where $it < $window_x | is-not-empty)
  let has_right = ($column_positions | where $it > $window_x | is-not-empty)
  let neighbor_count = (if $has_left { 1 } else { 0 }) + (if $has_right { 1 } else { 0 })
  let inter_column_gap = $neighbor_count * $gaps_in.w

  let available_space = $monitor.width - $gaps.w - $gaps.e - ($border_size * 2) - $inter_column_gap
  let window_scaled_width = $window_width * $monitor.scale

  print $"window: ($window_scaled_width), monitor: ($available_space)"
  if $window_scaled_width >= $available_space {
    print "making half size"
    hyprctl dispatch 'hl.dsp.layout("colresize 0.5")'
  } else {
    print "making full size"
    hyprctl dispatch 'hl.dsp.layout("colresize 1")'
  }

  # https://github.com/hyprwm/hyprland-plugins/issues/474
  # https://wiki.hypr.land/configuring/layouts/scrolling-layout/#layout-messages
}
