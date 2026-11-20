{
  anim,
  bezier,
  windCurves,
  ...
}:
{
  curves = windCurves // {
    overshot = bezier 0.05 0.9 0.1 1.05;
    smoothOut = bezier 0.5 0 0.99 0.99;
    smoothIn = bezier 0.5 (-0.5) 0.68 1.5;
  };
  animations = [
    (anim "windows" 6 "wind" "slide")
    (anim "windowsIn" 5 "winIn" "slide")
    (anim "windowsOut" 3 "smoothOut" "slide")
    (anim "windowsMove" 5 "wind" "slide")
    (anim "border" 1 "liner" null)
    (anim "fade" 3 "smoothOut" null)
    (anim "workspaces" 5 "overshot" null)
    (anim "workspacesIn" 5 "winIn" "slide")
    (anim "workspacesOut" 5 "winOut" "slide")
  ];
}
