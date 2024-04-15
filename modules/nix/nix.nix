{ outputs, pkgs, system, hostname, ... }:

outputs.lib.mkFor system hostname {
  common = {
    nix = {
      package = pkgs.nix;
      settings = {
        auto-optimise-store = true;
        warn-dirty = false;
        experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      };
    };
  };

  systems.darwin = {
    # aarch64-darwin can also run x86_64-darwin binaries with Rosetta 2,
    # so we can use the same config for all Darwin systems.
    nix.settings.extra-platforms = [ "x86_64-darwin" ];
  };
}
