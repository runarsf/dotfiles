_: {
  flake.homeModules.godot = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkEnableOption traceIf types;

    cfg = config.features.godot;
    android_cfg = config.features.android;
  in {
    options.features.godot = with types; {
      mono = mkEnableOption "Godot-Mono (C# support)";
    };

    config =
      traceIf (!android_cfg.enabled || false)
      "If exporting Godot projects to Android, make sure to enable the Android feature as well."
      {
        home.packages = with pkgs; (
          if cfg.mono
          then [godot-mono]
          else [godot]
        );
      };
  };
}
