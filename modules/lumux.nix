{
  ...
}:
{
  flake.nixosModules.lumux = { ... }: {
    services.flatpak = {
      packages = [
        # self.packages.${pkgs.stdenv.hostPlatform.system}.lumux
        "io.github.enginkirmaci.lumux"
      ];
    };
  };
}
