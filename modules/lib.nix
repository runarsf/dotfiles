{
  inputs,
  lib,
  ...
}: {
  _module.args.lib' = inputs.nixlib.lib.deepMerge [
    lib
    inputs.home-manager.lib
    inputs.nixlib.lib
    {
      useFeatures = self: features: {
        home =
          map (f: self.homeModules.${f})
          (builtins.filter (f: (self.homeModules or {}) ? ${f}) features);
        nixos =
          map (f: self.nixosModules.${f})
          (builtins.filter (f: (self.nixosModules or {}) ? ${f}) features);
      };
    }
  ];
}
