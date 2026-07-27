{inputs, ...}: {
  flake.nixosModules.zed = _: {
    nix.settings = rec {
      substituters = ["https://zed.cachix.org"];
      trusted-substituters = substituters;
      trusted-public-keys = ["zed.cachix.org-1:/pHQ6dpMsAZk2DiP4WCL0p9YDNKWj2Q5FL20bNmw1cU="];
    };
  };

  flake.homeModules.zed = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    programs = {
      zed-editor = {
        enable = true;
        # package = inputs.zed.packages.${system}.default;
      };
      television.enable = true;
    };

    home.packages = with pkgs; [
      ansifilter
    ];
  };
}
