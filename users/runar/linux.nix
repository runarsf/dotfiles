{
  self,
  inputs,
  ...
}: {
  flake.homeConfigurations.runar = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {system = "x86_64-linux";};
    extraSpecialArgs = {inherit self;};
    modules = [
      self.homeModules.runar
      {
        home.username = "runar";
        home.homeDirectory = "/home/runar";
      }
    ];
  };
}
