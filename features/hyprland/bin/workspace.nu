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
      $"hl.dsp.focus\({ monitor = \"($toMonitor)\" })"
      $"hl.dsp.focus\({ workspace = ($target), on_current_monitor = true })"
    ]
  } else {
    let visibleWorkspaces: list<string> = $monitors | get activeWorkspace.name

    if $target in $visibleWorkspaces {
      $cmdbuf ++= [ $"hl.dsp.focus\({ workspace = ($target) })" ]
    } else {
      $cmdbuf ++= [ $"hl.dsp.focus\({ workspace = ($target), on_current_monitor = true })" ]
    }
  }

  let cmd = $cmdbuf | each {|cmd| "dispatch " + $cmd} | str join "; "
  hyprctl --batch $cmd o> /dev/null
}
