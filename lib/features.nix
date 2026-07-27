_: let
  inherit (builtins) filter isString attrNames head;
in {
  libExtensions = [
    {
      useFeatures = self: features:
        let
          normalize = f:
            if isString f then { name = f; config = { }; }
            else let name = head (attrNames f); in { inherit name; config = f.${name}; };

          normalized = map normalize features;

          hasHome  = e: (self.homeModules  or { }) ? ${e.name};
          hasNixos = e: (self.nixosModules or { }) ? ${e.name};
          hasConfig = e: e.config != { };

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
