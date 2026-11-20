{ inputs, lib', ... }:
let
  inherit (lib')
    attrNames
    filter
    genAttrs
    hasInfix
    ;

  # Upstream is x86_64-linux only; version-pinned names (photocraft_0_3_0) are skipped.
  apps = filter (name: name != "default" && !hasInfix "_" name) (
    attrNames inputs.artcraft.packages.x86_64-linux
  );
in
{
  flake.nixosModules.artcraft = lib'.mkModule "artcraft" (
    { config, lib, ... }:
    let
      inherit (lib) mkEnableOption mkIf;

      cfg = config.modules.artcraft;

      enabled = filter (app: cfg.${app}.enable) apps;
    in
    {
      imports = [ inputs.artcraft.nixosModules.default ];

      options.modules.artcraft = genAttrs apps (app: {
        enable = mkEnableOption app;
      });

      config = mkIf (enabled != [ ]) {
        programs.artcraft = {
          enable = true;
          apps = enabled;
        };
      };
    }
  );
}
