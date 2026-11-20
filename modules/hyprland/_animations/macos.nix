{
  anim,
  bezier,
  spring,
  springAnim,
  ...
}:
{
  curves = {
    macOpen = spring 1 110 16;
    macBounce = spring 1 80 10;
    macSmooth = bezier 0.25 0.1 0.25 1.0;
  };
  animations = [
    (anim "fadeIn" 8 "default" null)
    (anim "fadeOut" 8 "default" null)
    (anim "fadeSwitch" 8 "default" null)
    (anim "fadeShadow" 8 "default" null)
    (anim "fadeDim" 8 "default" null)
    (anim "fadeLayers" 8 "default" null)
    (anim "fadeLayersIn" 8 "default" null)
    (anim "fadeLayersOut" 8 "default" null)
    (anim "fadePopups" 8 "default" null)
    (anim "fadePopupsIn" 8 "default" null)
    (anim "fadePopupsOut" 8 "default" null)
    (anim "fadeDpms" 8 "default" null)
    (springAnim "windows" 8 "macOpen" "popin 90%")
    (springAnim "windowsIn" 8 "macOpen" "popin 90%")
    (springAnim "windowsOut" 7 "macBounce" "popin 90%")
    (anim "windowsMove" 5 "macSmooth" "slide")
    (anim "border" 1 "default" null)
    (anim "borderangle" 30 "default" "once")
    (anim "fade" 8 "default" null)
    (anim "workspaces" 6 "macSmooth" "slidefade 20%")
    (anim "workspacesIn" 6 "macSmooth" "slidefade 20%")
    (anim "workspacesOut" 6 "macSmooth" "slidefade 20%")
    (anim "specialWorkspace" 6 "macSmooth" "slidefade 20%")
    (anim "specialWorkspaceIn" 6 "macSmooth" "slidefade 20%")
    (anim "specialWorkspaceOut" 6 "macSmooth" "slidefade 20%")
    (anim "zoomFactor" 7 "default" null)
    (anim "monitorAdded" 7 "default" null)
  ];
}
