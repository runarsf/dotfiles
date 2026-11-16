{
  anim,
  bezier,
  ...
}:
{
  curves.quart = bezier 0.25 1 0.5 1;
  animations = [
    (anim "windowsIn" 6 "quart" "slide")
    (anim "windowsOut" 6 "quart" "slide")
    (anim "windowsMove" 6 "quart" "slide")
    (anim "fadeIn" 6 "quart" null)
    (anim "fadeOut" 6 "quart" null)
    (anim "fadeSwitch" 6 "quart" null)
    (anim "fadeShadow" 6 "quart" null)
    (anim "fadeDim" 6 "quart" null)
    (anim "fadeLayers" 6 "quart" null)
    (anim "fadePopups" 6 "quart" null)
    (anim "fadeDpms" 6 "quart" null)
    (anim "workspacesIn" 6 "quart" null)
    (anim "workspacesOut" 6 "quart" null)
    (anim "specialWorkspace" 6 "quart" null)
    (anim "windows" 6 "quart" "slide")
    (anim "border" 6 "quart" null)
    (anim "borderangle" 6 "quart" null)
    (anim "fade" 6 "quart" null)
    (anim "workspaces" 6 "quart" null)
    (anim "fadeLayersIn" 6 "quart" null)
    (anim "fadeLayersOut" 6 "quart" null)
    (anim "fadePopupsIn" 6 "quart" null)
    (anim "fadePopupsOut" 6 "quart" null)
    (anim "specialWorkspaceIn" 6 "quart" null)
    (anim "specialWorkspaceOut" 6 "quart" null)
  ];
}
