def from-css [] {
  $in | split words | zip [n e s w] | reduce -f {} {|it, acc| $acc | insert $it.1 ($it.0 | into int)}
}

def main [] {
  let window = hyprctl -j activewindow | from json

  if ($window | is-empty) or $window.floating {
    return
  }

  let monitors = hyprctl -j monitors | from json | where id == $window.monitor
  if ($monitors | is-empty) {
    return
  }
  let monitor = $monitors | first

  let gaps = hyprctl -j getoption general.gaps_out | from json | get css | from-css
  let border_size = hyprctl -j getoption general.border_size | from json | get int

  # transforms 1/3/5/7 are the 90/270 rotations, where the mode's width/height are swapped
  let monitor_width = if ($monitor.transform mod 2) == 1 { $monitor.height } else { $monitor.width }
  # reserved is [left top right bottom] in logical units, like the gaps and the border
  let reserved = ($monitor.reserved | get 0) + ($monitor.reserved | get 2)
  let usable_width = ($monitor_width / $monitor.scale) - $reserved - $gaps.w - $gaps.e - ($border_size * 2)

  # a fully-resized column loses a few more pixels to the inner gap of each neighboring
  # column, so compare against a fraction instead of an exact width
  if $window.size.0 >= ($usable_width * 0.95) {
    hyprctl dispatch 'hl.dsp.layout("colresize 0.5")'
  } else {
    hyprctl dispatch 'hl.dsp.layout("colresize 1")'
  }

  # https://github.com/hyprwm/hyprland-plugins/issues/474
  # https://wiki.hypr.land/configuring/layouts/scrolling-layout/#layout-messages
}
