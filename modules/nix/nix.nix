{ outputs, pkgs, system, hostname, ... }:

outputs.lib.mkFor system hostname {
  common = {
    nix = {
      package = pkgs.nix;
      settings = {
        auto-optimise-store = true;
        warn-dirty = false;
        experimental-features = [ "nix-command" "flakes" "repl-flake" ];
        # TODO Set this in hyprland config
        substituters =
          [ "https://cache.nixos.org" "https://hyprland.cachix.org" ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        ];
      };
    };
  };

  systems.darwin = {
    # aarch64-darwin can also run x86_64-darwin binaries with Rosetta 2,
    # so we can use the same config for all Darwin systems.
    nix.settings.extra-platforms = [ "x86_64-darwin" ];
  };
}
