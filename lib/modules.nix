{ inputs, lib, ... }:
let
  inherit (builtins) filter isString concatMap;
  inherit (lib) mapAttrsToList;

  # Imports a module under the key useModules gives it, so reaching
  # it from both a module list and another module's imports loads it once.
  importModule = self: kind: name: {
    key = "module:${kind}:${name}";
    imports = [ self."${kind}Modules".${name} ];
  };
in
{
  inherit importModule;

  # What useModules takes: names, or attrsets of name -> module config.
  modulesType = lib.types.listOf (lib.types.either lib.types.str (lib.types.attrsOf lib.types.raw));

  # Wraps a module and declares a read-only `modules.<name>.enable`
  # option alongside it, set to `true` whenever the module is imported. Lets
  # other modules check `config.modules.<name>.enable or false` to react
  # to a module being active without having to import it directly.
  #
  # If `arg` has no `wants`, it *is* the module (function or attrset):
  #   mkModule "wayland" ({ pkgs, lib, osConfig, ... }: { ... })
  # If it declares `wants` (a list of module names this one wants enabled
  # alongside it), the module goes under `config` instead, and a warning is
  # emitted (build-time only, nothing is disabled) for each unmet want:
  #   mkModule "hyprland" { wants = [ "wayland" ]; config = { ... }: { ... }; }
  # `needs` is the hard version: each name is imported, so its options are
  # declared and this module can read or set them. Use it when `config` would
  # fail without the other module, `wants` when it only degrades. A name in
  # both module sets is ambiguous and throws; import it with `importModule`.
  # `wants` checks `config.modules.<dep>.enable` first (same-evaluation
  # modules), falling back to `osConfig.modules.<dep>.enable` (a
  # nixos-only module, seen from a home module running under that nixos
  # config). It cannot see a home-only module from a nixos module, since
  # nixos config has no visibility into a specific home-manager user's
  # config without naming that user.
  mkModule =
    name: arg:
    let
      wrapped = arg ? wants || arg ? needs;
      wants = arg.wants or [ ];
      module =
        if wrapped then arg.config or (throw "mkModule '${name}': missing 'config' attribute") else arg;

      needs = map (
        dep:
        let
          kinds = filter (kind: (inputs.self."${kind}Modules" or { }) ? ${dep}) [
            "home"
            "nixos"
          ];
        in
        if kinds == [ ] then
          throw "mkModule '${name}': needs unknown module '${dep}'"
        else if kinds != [ (builtins.head kinds) ] then
          throw "mkModule '${name}': needs '${dep}', which is both a home and a nixos module; import the one you mean with lib'.importModule"
        else
          importModule inputs.self (builtins.head kinds) dep
      ) (arg.needs or [ ]);
    in
    {
      imports = [
        module
        ({ lib, ... }: {
          options.modules.${name}.enable = lib.mkOption {
            type = lib.types.bool;
            readOnly = true;
            default = true;
            description = "Whether the '${name}' module has been imported into this configuration.";
          };
        })
      ]
      ++ needs
      ++ (
        if wants == [ ] then
          [ ]
        else
          [
            (
              {
                config,
                lib,
                osConfig ? { },
                ...
              }:
              {
                config.warnings = lib.concatMap (
                  dep:
                  lib.optional (
                    !(config.modules.${dep}.enable or osConfig.modules.${dep}.enable or false)
                  ) "modules.${name} wants the '${dep}' module to be enabled, but it isn't for this configuration."
                ) wants;
              }
            )
          ]
      );
    };

  useModules =
    self: modules:
    let
      # An entry is either a bare name or an attrset of name -> config, so one
      # attrset may declare several modules at once.
      normalize =
        f:
        if isString f then
          [
            {
              name = f;
              config = { };
            }
          ]
        else
          mapAttrsToList (name: config: { inherit name config; }) f;

      hasHome = e: (self.homeModules or { }) ? ${e.name};
      hasNixos = e: (self.nixosModules or { }) ? ${e.name};
      hasConfig = e: e.config != { };

      normalized = map (
        e:
        if hasHome e || hasNixos e then
          e
        else
          throw "useModules: module '${e.name}' has neither a homeModules nor a nixosModules entry"
      ) (concatMap normalize modules);

      # A stable key lets the module system dedupe a module reached from more
      # than one list (a user's and their host's). Without it the module is
      # evaluated twice and unique options conflict with themselves.
      importOne = importModule self;

      configModule =
        name: cfg:
        {
          lib,
          options,
          ...
        }:
        {
          config =
            let
              declared = lib.attrByPath [ "modules" name ] { } options;
              filtered = lib.filterAttrs (k: _: declared ? ${k}) cfg;
            in
            lib.optionalAttrs (filtered != { }) { modules.${name} = filtered; };
        };
    in
    {
      home =
        map (e: importOne "home" e.name) (filter hasHome normalized)
        ++ map (e: configModule e.name e.config) (filter (e: hasHome e && hasConfig e) normalized);
      nixos =
        map (e: importOne "nixos" e.name) (filter hasNixos normalized)
        ++ map (e: configModule e.name e.config) (filter (e: hasNixos e && hasConfig e) normalized);
    };
}
