#! /usr/bin/env cached-nix-shell
#! nix-shell -i nu -p nushell

def main [target: string] {
  let monitors: list<string> = hyprctl -j monitors | from json
  let monitorRules: list<string> = (hyprctl -j workspacerules | from json | where {
    |it| "monitor" in $it
  } | update monitor {
    |$it| $it.monitor | split row ";"
  } | flatten | where monitor in ($monitors | get name))

  mut cmdbuf: list<string> = []

  if $target in ($monitorRules | get workspaceString) {
    let toMonitor: string = $monitorRules | where workspaceString == $target | get monitor | first
    $cmdbuf ++= [
      $"focusmonitor ($toMonitor)"
      $"focusworkspaceoncurrentmonitor ($target)"
    ]
  } else {
    let visibleWorkspaces: list<string> = $monitors | get activeWorkspace.name

    if $target in $visibleWorkspaces {
      $cmdbuf ++= [ $"workspace ($target)" ]
    } else {
      $cmdbuf ++= [ $"focusworkspaceoncurrentmonitor ($target)" ]
    }
  }

  let cmd = $cmdbuf | each {|cmd| "dispatch " + $cmd} | str join "; "
  hyprctl --batch $cmd o> /dev/null
}
