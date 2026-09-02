{lib', ...}: let
  # inherit (lib) optionals;
  inherit (lib') mkFeature;
  # cfg = config.features.godot;
in {
  flake.homeModules.godot = mkFeature "godot" ({
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkEnableOption types;

    cfg = config.features.godot;
  in {
    options.features.godot = with types; {
      mono = mkEnableOption "Godot-Mono (C# support)";
      android = mkEnableOption "Android Export";
    };

    config = {
      home.packages = with pkgs; (
        if cfg.mono
        then [godot-mono]
        else [godot]
      );
    };
  });
}
