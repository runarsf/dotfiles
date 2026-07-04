{inputs, ...}: {
  imports = [
    inputs.home-manager.flakeModules.home-manager
    inputs.treefmt-nix.flakeModule
  ];

  config = {
    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    perSystem = {
      system,
      config,
      ...
    }: {
      treefmt.imports = [./treefmt.nix];
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        config.allowUnfreePredicate = _: true;

        overlays = [
          (_: prev: {
            master = import inputs.nixpkgs-master {
              inherit system;
              config.allowUnfree = true;
            };
            nur = import inputs.nur {
              pkgs = prev;
              nurpkgs = import inputs.nixpkgs {inherit (prev) system config overlays;};
            };
          })
        ];
      };
    };
  };
}
