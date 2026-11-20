{
  anim,
  bezier,
  spring,
  springAnim,
  ...
}:
{
  curves = {
    gnomeOpen = spring 1 100 14;
    gnomeClose = spring 1 90 16;
    gnomeFade = bezier 0.25 0.1 0.25 1.0;
  };
  animations = [
    (springAnim "windowsIn" 7 "gnomeOpen" "gnomed")
    (springAnim "windowsOut" 6 "gnomeClose" "gnomed")
    (anim "windowsMove" 5 "gnomeFade" "slide")
    (anim "border" 1 "default" null)
    (anim "borderangle" 30 "default" "once")
    (anim "fade" 9 "default" null)
    (anim "workspaces" 6 "gnomeFade" "slidefade 20%")
    (anim "workspacesIn" 6 "gnomeFade" "slidefade 20%")
    (anim "workspacesOut" 6 "gnomeFade" "slidefade 20%")
    (anim "specialWorkspace" 6 "gnomeFade" "slidefade 20%")
    (anim "specialWorkspaceIn" 6 "gnomeFade" "slidefade 20%")
    (anim "specialWorkspaceOut" 6 "gnomeFade" "slidefade 20%")
    (anim "zoomFactor" 7 "default" null)
    (anim "monitorAdded" 7 "default" null)
  ];
}
