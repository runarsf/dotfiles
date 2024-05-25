{ inputs, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
      permittedInsecurePackages = [
        "electron-25.9.0"
      ];
    };
    overlays = [
      (_: prev: {
        master = import inputs.nixpkgs-master {
          inherit (prev) system config overlays;
        };
        unstable = import inputs.nixpkgs-unstable {
          inherit (prev) system config overlays;
        };
        nur = import inputs.nur {
          pkgs = prev;
          nurpkgs = import inputs.nixpkgs {
            inherit (prev) system config overlays;
          };
        };
      })

      (_: prev:
        import ../../packages { pkgs = prev.pkgs.unstable; }
      )
    ];
  };
}
