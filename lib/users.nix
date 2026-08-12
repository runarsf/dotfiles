{inputs, ...}: {
  libExtensions = [
    {
      mkUser = {
        self,
        username,
        homeModule,
        homeDirectory ? "/home/${username}",
        system ? "x86_64-linux",
      }:
        inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs {inherit system;};
          extraSpecialArgs = {inherit self;};
          modules = [
            homeModule
            {
              home = {
                inherit username homeDirectory;
              };
            }
          ];
        };
    }
  ];
}
