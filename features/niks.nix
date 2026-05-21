{
  self,
  inputs,
  ...
}: {
  flake.homeModules.niks = {
    pkgs,
    lib,
    config,
    ...
  }: {
    options = {
      features.niks = {
        flake = lib.mkOption {
          type = lib.types.str;
          default = "${config.home.homeDirectory}/.config/nixos";
          description = "Path to the NixOS flake, set as NH_FLAKE.";
        };
      };
    };

    config = {
      home.packages = [self.packages.${pkgs.stdenv.hostPlatform.system}.niks];
      home.sessionVariables.NH_FLAKE = config.features.niks.flake;
    };
  };
}
