{
  anim,
  bezier,
  ...
}:
{
  curves = {
    wind = bezier 0.05 0.85 0.03 0.97;
    winIn = bezier 0.07 0.88 0.04 0.99;
    winOut = bezier 0.20 (-0.15) 0 1;
    liner = bezier 1 1 1 1;
    md3_standard = bezier 0.12 0 0 1;
    md3_decel = bezier 0.05 0.80 0.10 0.97;
    md3_accel = bezier 0.20 0 0.80 0.08;
    overshot = bezier 0.05 0.85 0.07 1.04;
    crazyshot = bezier 0.1 1.22 0.68 0.98;
    hyprnostretch = bezier 0.05 0.82 0.03 0.94;
    menu_decel = bezier 0.05 0.82 0 1;
    menu_accel = bezier 0.20 0 0.82 0.10;
    easeInOutCirc = bezier 0.75 0 0.15 1;
    easeOutCirc = bezier 0 0.48 0.38 1;
    easeOutExpo = bezier 0.10 0.94 0.23 0.98;
    softAcDecel = bezier 0.20 0.20 0.15 1;
    md2 = bezier 0.30 0 0.15 1;
    OutBack = bezier 0.28 1.40 0.58 1;
  };
  animations = [
    (anim "fadeIn" 1.8 "md3_decel" null)
    (anim "fadeOut" 1.8 "md3_decel" null)
    (anim "fadeSwitch" 1.8 "md3_decel" null)
    (anim "fadeShadow" 1.8 "md3_decel" null)
    (anim "fadeDim" 1.8 "md3_decel" null)
    (anim "fadeLayers" 1.8 "md3_decel" null)
    (anim "fadePopups" 1.8 "md3_decel" null)
    (anim "fadeDpms" 1.8 "md3_decel" null)
    (anim "workspacesIn" 4.0 "menu_decel" "slide")
    (anim "workspacesOut" 4.0 "menu_decel" "slide")
    (anim "specialWorkspaceIn" 2.3 "md3_decel" "slidefadevert 15%")
    (anim "specialWorkspaceOut" 2.3 "md3_decel" "slidefadevert 15%")
    (anim "border" 1.6 "liner" null)
    (anim "borderangle" 82 "liner" "loop")
    (anim "windowsIn" 3.2 "winIn" "slide")
    (anim "windowsOut" 2.8 "easeOutCirc" null)
    (anim "windowsMove" 3.0 "wind" "slide")
    (anim "fade" 1.8 "md3_decel" null)
    (anim "layersIn" 1.8 "menu_decel" "slide")
    (anim "layersOut" 1.5 "menu_accel" null)
    (anim "fadeLayersIn" 1.6 "menu_decel" null)
    (anim "fadeLayersOut" 1.8 "menu_accel" null)
    (anim "workspaces" 4.0 "menu_decel" "slide")
    # Upstream sets specialWorkspace twice; the second call wins.
    (anim "specialWorkspace" 4.0 "menu_decel" "slide")
    (anim "specialWorkspace" 2.3 "md3_decel" "slidefadevert 15%")
    (anim "fadePopupsIn" 1.8 "md3_decel" null)
    (anim "fadePopupsOut" 1.8 "md3_decel" null)
  ];
}
