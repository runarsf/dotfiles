# Animation presets
# https://github.com/mylinuxforwork/dotfiles/tree/main/dotfiles/.config/hypr/conf/animations
# https://github.com/HyDE-Project/HyDE/tree/master/Configs/.local/share/hypr/lua/animations
{
  config,
  lib,
  osConfig,
  ...
}:
let
  cfg = config.modules.hyprland;

  bezier = x1: y1: x2: y2: {
    type = "bezier";
    points = [
      [ x1 y1 ]
      [ x2 y2 ]
    ];
  };

  spring = mass: stiffness: dampening: {
    type = "spring";
    inherit mass stiffness dampening;
  };

  mkAnim =
    key: leaf: speed: curve: style:
    {
      inherit leaf speed;
      enabled = true;
      ${key} = curve;
    }
    // lib.optionalAttrs (style != null) { inherit style; };

  helpers = {
    inherit bezier spring;

    anim = mkAnim "bezier";
    springAnim = mkAnim "spring";
    noAnim = leaf: {
      inherit leaf;
      enabled = false;
    };

    # Curve sets shared by several presets.
    md3Curves = {
      linear = bezier 0 0 1 1;
      md3_standard = bezier 0.2 0 0 1;
      md3_decel = bezier 0.05 0.7 0.1 1;
      md3_accel = bezier 0.3 0 0.8 0.15;
      overshot = bezier 0.05 0.9 0.1 1.1;
      crazyshot = bezier 0.1 1.5 0.76 0.92;
      hyprnostretch = bezier 0.05 0.9 0.1 1.0;
      easeInOutCirc = bezier 0.85 0 0.15 1;
      easeOutCirc = bezier 0 0.55 0.45 1;
      easeOutExpo = bezier 0.16 1 0.3 1;
    };

    menuCurves = {
      menu_decel = bezier 0.1 1 0 1;
      menu_accel = bezier 0.38 0.04 1 0.07;
      softAcDecel = bezier 0.26 0.26 0.15 1;
      md2 = bezier 0.4 0 0.2 1;
    };

    windCurves = {
      wind = bezier 0.05 0.9 0.1 1.05;
      winIn = bezier 0.1 1.1 0.1 1.1;
      winOut = bezier 0.3 (-0.3) 0 1;
      liner = bezier 1 1 1 1;
    };
  };

  presets = lib.mapAttrs' (
    file: _:
    lib.nameValuePair (lib.removeSuffix ".nix" file) (import (./. + "/${file}") helpers)
  ) (lib.filterAttrs (file: type: type == "regular" && file != "default.nix") (builtins.readDir ./.));

  preset = presets.${cfg.animationPreset};
in
{
  options.modules.hyprland.animationPreset = lib.mkOption {
    type = lib.types.enum (lib.attrNames presets);
    default = "classic";
    example = "end4";
    description = "Which animation preset to use. `disabled` turns animations off entirely.";
  };

  config = lib.mkIf (osConfig.host.desktop or true) {
    wayland.windowManager.hyprland.settings = {
      config.animations.enabled = preset.enabled or true;
      curve = lib.mapAttrsToList (name: curve: { _args = [ name curve ]; }) preset.curves;
      animation = preset.animations;
    };
  };
}
