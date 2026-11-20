{
  inputs,
  lib',
  ...
}:
let
  # inherit (lib) optionals;
  inherit (lib') mkModule;
  # cfg = config.modules.godot;
in
{
  flake.homeModules.godot = mkModule "godot" (
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib) mkEnableOption mkOption types;
      inherit (pkgs.stdenv.hostPlatform) system;

      cfg = config.modules.godot;
      mv = inputs.nixpkgs-multiverse.multiverse."${system}".versions;
    in
    {
      imports = [ inputs.nixpkgs-multiverse.homeManagerModules.default ];

      options.modules.godot = with types; {
        mono = mkEnableOption "Godot-Mono (C# support)";
        android = mkEnableOption "Android Export";
        pinnedVersion = mkOption {
          type = str;
          default = "4.7.2-stable";
        };
      };

      config = {
        multiverse = {
          enable = true;
          config.allowUnfree = true;
        };

        home.packages =
          with pkgs;
          (
            if cfg.mono then
              [ mv."godot-mono"."${cfg.pinnedVersion}" ]
            else
              [ mv."godot"."${cfg.pinnedVersion}" ]
          );
      };
    }
  );
}
