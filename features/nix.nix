{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.nix = _: {
    nix.settings = rec {
      auto-optimise-store = true;
      warn-dirty = false;
      experimental-features = ["nix-command" "flakes" "pipe-operators"];
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
        "https://cache.garnix.io"
      ];
      trusted-substituters = substituters;
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
      ];
    };
  };
}
