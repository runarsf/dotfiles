{ self, ... }: {
  flake.homeModules.yaak =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
    in
    {
      home.packages = with pkgs; [
        # NOTE: Version should match; update yaak-cli when nixpkgs gets updated
        yaak
        self.packages.${system}.yaak-cli
      ];
    };
}
