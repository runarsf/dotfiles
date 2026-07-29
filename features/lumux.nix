{
  lib',
  self,
  ...
}: {
  flake.nixosModules.lumux = { config, pkgs, lib, ... }: {
    services.flatpak = {
      packages = [
        # self.packages.${pkgs.stdenv.hostPlatform.system}.lumux
        "io.github.enginkirmaci.lumux"
      ];
    };
  };
}
