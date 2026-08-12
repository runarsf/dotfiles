_: let
  inherit (builtins) filter isString attrNames head;
in {
  libExtensions = [
    {
      # Wraps a feature module and declares a read-only `features.<name>.enable`
      # option alongside it, set to `true` whenever the module is imported. Lets
      # other features check `config.features.<name>.enable or false` to react
      # to a feature being active without having to import it directly.
      #
      # If `arg` has no `wants`, it *is* the module (function or attrset):
      #   mkFeature "wayland" ({ pkgs, lib, osConfig, ... }: { ... })
      # If it declares `wants` (a list of feature names this one wants enabled
      # alongside it), the module goes under `config` instead, and a warning is
      # emitted (build-time only, nothing is disabled) for each unmet want:
      #   mkFeature "hyprland" { wants = [ "wayland" ]; config = { ... }: { ... }; }
      # `wants` checks `config.features.<dep>.enable` first (same-evaluation
      # features), falling back to `osConfig.features.<dep>.enable` (a
      # nixos-only feature, seen from a home module running under that nixos
      # config). It cannot see a home-only feature from a nixos module, since
      # nixos config has no visibility into a specific home-manager user's
      # config without naming that user.
      mkFeature = name: arg: let
        hasWants = arg ? wants;
        wants = arg.wants or [];
        module =
          if hasWants
          then arg.config or (throw "mkFeature '${name}': missing 'config' attribute")
          else arg;
      in {
        imports =
          [
            module
            (
              { lib, ... }:
              {
                options.features.${name}.enable = lib.mkOption {
                  type = lib.types.bool;
                  readOnly = true;
                  default = true;
                  description = "Whether the '${name}' feature module has been imported into this configuration.";
                };
              }
            )
          ]
          ++ (
            if wants == []
            then []
            else [
              (
                {
                  config,
                  lib,
                  osConfig ? {},
                  ...
                }:
                {
                  config.warnings = lib.concatMap (
                    dep:
                    lib.optional (!(config.features.${dep}.enable or osConfig.features.${dep}.enable or false))
                      "features.${name} wants the '${dep}' feature to be enabled, but it isn't for this configuration."
                  ) wants;
                }
              )
            ]
          );
      };

      useFeatures = self: features:
        let
          normalize = f:
            if isString f then { name = f; config = { }; }
            else let name = head (attrNames f); in { inherit name; config = f.${name}; };

          hasHome  = e: (self.homeModules  or { }) ? ${e.name};
          hasNixos = e: (self.nixosModules or { }) ? ${e.name};
          hasConfig = e: e.config != { };

          normalized = map (
            e:
            if hasHome e || hasNixos e
            then e
            else throw "useFeatures: feature '${e.name}' has neither a homeModules nor a nixosModules entry"
          ) (map normalize features);

          configModule = name: cfg:
            { lib, options, ... }: {
              config =
                let
                  declared = lib.attrByPath [ "features" name ] { } options;
                  filtered = lib.filterAttrs (k: _: declared ? ${k}) cfg;
                in
                lib.optionalAttrs (filtered != { }) { features.${name} = filtered; };
            };
        in
        {
          home =
            map (e: self.homeModules.${e.name})  (filter hasHome  normalized)
            ++ map (e: configModule e.name e.config) (filter (e: hasHome  e && hasConfig e) normalized);
          nixos =
            map (e: self.nixosModules.${e.name}) (filter hasNixos normalized)
            ++ map (e: configModule e.name e.config) (filter (e: hasNixos e && hasConfig e) normalized);
        };
    }
  ];
}
