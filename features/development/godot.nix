_: {
  flake.homeModules.godot = {
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
    };

    config = {
      home.packages = with pkgs; (
        if cfg.mono
        then [godot-mono]
        else [godot]
      );
    };
  };
}
