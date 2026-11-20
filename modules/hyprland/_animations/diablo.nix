{
  anim,
  bezier,
  ...
}:
{
  curves = {
    default = bezier 0.05 0.9 0.1 1.05;
    wind = bezier 0.05 0.9 0.1 1.05;
    overshot = bezier 0.13 0.99 0.29 1.08;
    liner = bezier 1 1 1 1;
  };
  animations = [
    (anim "layersIn" 5 "default" "popin")
    (anim "layersOut" 5 "default" "popin")
    (anim "fadeLayersIn" 10 "default" null)
    (anim "fadeLayersOut" 10 "default" null)
    (anim "workspacesIn" 7 "overshot" "slidevert")
    (anim "workspacesOut" 7 "overshot" "slidevert")
    (anim "specialWorkspace" 7 "overshot" "slidevert")
    (anim "windows" 7 "wind" "popin")
    (anim "windowsIn" 7 "overshot" "popin")
    (anim "windowsOut" 5 "overshot" "popin")
    (anim "windowsMove" 6 "overshot" "slide")
    (anim "layers" 5 "default" "popin")
    (anim "fadeIn" 10 "default" null)
    (anim "fadeOut" 10 "default" null)
    (anim "fadeSwitch" 10 "default" null)
    (anim "fadeShadow" 10 "default" null)
    (anim "fadeDim" 10 "default" null)
    (anim "fadeLayers" 10 "default" null)
    (anim "workspaces" 7 "overshot" "slidevert")
    (anim "border" 1 "liner" null)
    (anim "borderangle" 30 "liner" "loop")
    (anim "specialWorkspaceIn" 7 "overshot" "slidevert")
    (anim "specialWorkspaceOut" 7 "overshot" "slidevert")
  ];
}
