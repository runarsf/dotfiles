{inputs, ...}: {
  imports = [
    # adds home-manager options to flake-parts
    inputs.home-manager.flakeModules.home-manager
  ];

  config = {
    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];

    perSystem = {system, ...}: {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        config.allowUnfreePredicate = _: true;

        overlays = [
          (_: _: {
            master = import inputs.nixpkgs-master {
              inherit system;
              config.allowUnfree = true;
            };
          })
        ];
      };
    };
  };
}
