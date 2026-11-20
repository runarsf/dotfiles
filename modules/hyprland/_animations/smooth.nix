{
  anim,
  bezier,
  ...
}:
{
  curves = {
    overshot = bezier 0.05 0.9 0.1 1.05;
    easeOutExpo = bezier 0.16 1 0.3 1;
    easeOutBack = bezier 0.34 1.56 0.64 1;
    easeInBack = bezier 0.36 0 0.66 (-0.56);
    easeInOutBack = bezier 0.68 (-0.6) 0.32 1.6;
  };
  animations = [
    (anim "windows" 3 "overshot" "slide")
    (anim "windowsOut" 100 "easeOutExpo" null)
    (anim "windowsIn" 7 "easeOutBack" null)
    (anim "windowsMove" 6 "easeInOutBack" "slide")
    (anim "border" 5 "default" null)
    (anim "fade" 5 "easeOutBack" null)
    (anim "fadeDim" 5 "easeOutBack" null)
    (anim "workspaces" 10 "easeOutExpo" null)
  ];
}
