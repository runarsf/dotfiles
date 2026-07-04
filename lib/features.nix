_: let
  inherit (builtins) filter;
in {
  libExtensions = [
    {
      useFeatures = self: features: {
        home =
          map (f: self.homeModules.${f})
          (filter (f: (self.homeModules or {}) ? ${f}) features);
        nixos =
          map (f: self.nixosModules.${f})
          (filter (f: (self.nixosModules or {}) ? ${f}) features);
      };
    }
  ];
}
