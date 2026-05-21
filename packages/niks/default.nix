_: {
  perSystem = {pkgs, ...}: {
    packages.niks = pkgs.writers.writeNuBin "niks" (builtins.readFile ./bin/niks.nu);
  };
}
