{
  anim,
  bezier,
  ...
}:
{
  curves.myBezier = bezier 0.05 0.9 0.1 1.05;
  animations = [
    (anim "windows" 7 "myBezier" null)
    (anim "windowsOut" 7 "default" "popin 80%")
    (anim "border" 10 "default" null)
    (anim "borderangle" 8 "default" null)
    (anim "fade" 7 "default" null)
    (anim "workspaces" 6 "default" null)
  ];
}
