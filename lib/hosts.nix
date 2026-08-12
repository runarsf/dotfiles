{inputs, ...}: {
  libExtensions = [
    {
      mkHost = {
        self,
        withSystem,
        configuration,
        system ? "x86_64-linux",
        extraModules ? [],
      }:
        withSystem system (
          {pkgs, ...}:
            inputs.nixpkgs.lib.nixosSystem {
              inherit pkgs;
              modules =
                [
                  configuration
                  self.nixosModules.homeManager
                ]
                ++ extraModules;
            }
        );
    }
  ];
}
