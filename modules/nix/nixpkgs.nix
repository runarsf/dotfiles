{ inputs, ... }:
{
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = _: true;
      permittedInsecurePackages = [
        "electron-25.9.0"
        # TODO I have no clue where this one is required
        "dotnet-sdk-6.0.428"
        "dotnet-sdk-7.0.410"
        "qtwebengine-5.15.19"
      ];
    };
    overlays = [
      (_: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit (prev) system config overlays;
        };
        master = import inputs.nixpkgs-master {
          inherit (prev) system config overlays;
        };
        nur = import inputs.nur {
          pkgs = prev;
          nurpkgs = import inputs.nixpkgs { inherit (prev) system config overlays; };
        };
      })

      (_: prev: {
        inherit
          (import inputs.nixpkgs-stremio {
            inherit (prev) config;
            system = prev.stdenv.hostPlatform.system;
          })
          stremio
          ;
      })

      (
        _: prev:
        import ../../packages {
          inherit (prev) pkgs;
          inherit inputs;
        }
      )
    ];
  };
}
