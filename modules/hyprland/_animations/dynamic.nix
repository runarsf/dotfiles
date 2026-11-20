{
  anim,
  windCurves,
  ...
}:
{
  curves = windCurves;
  animations = [
    (anim "windows" 6 "wind" "slide")
    (anim "windowsIn" 6 "winIn" "slide")
    (anim "windowsOut" 5 "winOut" "slide")
    (anim "windowsMove" 5 "wind" "slide")
    (anim "border" 1 "liner" null)
    (anim "borderangle" 30 "liner" "loop")
    (anim "fade" 10 "default" null)
    (anim "workspaces" 5 "wind" null)
  ];
}
