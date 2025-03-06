#! /usr/bin/env cached-nix-shell
#! nix-shell -i nu -p nushell

def main [
  terminal: string,
  class: string,
  ...tabbed: string,
  --width (-w): int = 1050,
  --height (-h): int = 675,
  --workspace (-W): string = "scratchpad",
  --sleep (-s): duration = 1500ms,
] {
  hyprctl dispatch exec $"[size ($width) ($height)] ($terminal)" o> /dev/null

  sleep $sleep

  let clients: table = hyprctl -j clients | from json
  let address: string = $clients | where class == $class and workspace.name == $"special:($workspace)" | first | get address

  mut cmdbuf: list<string> = [
    $"dispatch focuswindow address:($address)",
    "dispatch togglegroup"
  ]
  for $tab in $tabbed {
    $cmdbuf = $cmdbuf | append $"dispatch exec ($tab)"
  }

  # TODO Force focus to scratchpad while doing operations?
  let cmd = $cmdbuf | str join "; "
  hyprctl --batch $cmd o> /dev/null

  sleep $sleep

  hyprctl --batch $"dispatch focuswindow address:($address); dispatch lockactivegroup lock" o> /dev/null
}
