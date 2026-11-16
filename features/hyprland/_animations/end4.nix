{
  anim,
  md3Curves,
  menuCurves,
  ...
}:
{
  curves = md3Curves // menuCurves;
  animations = [
    (anim "windows" 3 "md3_decel" "popin 60%")
    (anim "windowsIn" 3 "md3_decel" "popin 60%")
    (anim "windowsOut" 3 "md3_accel" "popin 60%")
    (anim "border" 10 "default" null)
    (anim "fade" 3 "md3_decel" null)
    (anim "layersIn" 3 "menu_decel" "slide")
    (anim "layersOut" 1.6 "menu_accel" null)
    (anim "fadeLayersIn" 2 "menu_decel" null)
    (anim "fadeLayersOut" 4.5 "menu_accel" null)
    (anim "workspaces" 7 "menu_decel" "slide")
    (anim "specialWorkspace" 3 "md3_decel" "slidevert")
  ];
}
