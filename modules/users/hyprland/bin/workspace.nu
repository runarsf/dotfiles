#! /usr/bin/env cached-nix-shell
#! nix-shell -i nu -p nushell

# TODO Having multiple monitors for one workspace is not supported,
# fina a different way to do it.
def main [target: string] {
  let monitors: list<string> = hyprctl -j monitors | from json
  let monitorRules: list<string> = (hyprctl -j workspacerules | from json | where {
    |rec| "monitor" in $rec and $rec.monitor in ($monitors | get name)
  })

  mut cmdbuf: list<string> = []

  if $target in ($monitorRules | get workspaceString) {
    let toMonitor: string = $monitorRules | where workspaceString == $target | get monitor | first
    $cmdbuf = $cmdbuf | append $"focusmonitor ($toMonitor)"
    $cmdbuf = $cmdbuf | append $"focusworkspaceoncurrentmonitor ($target)"
  } else {
    let visibleWorkspaces: list<string> = $monitors | get activeWorkspace.name

    if $target in $visibleWorkspaces {
      $cmdbuf = $cmdbuf | append $"workspace ($target)"
    } else {
      $cmdbuf = $cmdbuf | append $"focusworkspaceoncurrentmonitor ($target)"
    }
  }

  let cmd = $cmdbuf | each {|cmd| "dispatch " + $cmd} | str join "; "
  hyprctl --batch $cmd o> /dev/null
}
