{
  outputs,
  pkgs,
  system,
  hostname,
  users,
  ...
}:
outputs.lib.mkFor system hostname {
  common = {
    nix = {
      package = pkgs.nix;
      settings = {
        auto-optimise-store = true;
        warn-dirty = false;
        experimental-features = ["nix-command" "flakes" "pipe-operators"];
        # TODO Set this in hyprland config
        substituters = [
          "https://cache.nixos.org/"
          "https://hyprland.cachix.org"
          "https://wezterm.cachix.org"
          "https://cache.thalheim.io"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
          "wezterm.cachix.org-1:kAbhjYUC9qvblTE+s7S+kl5XM1zVa4skO+E/1IDWdH0="
          "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
        ];

        # if you want this to work with home-manager-only setups, you need to
        # manually add the user to the trusted-users list in /etc/nix/nix.conf
        # since home-manager itself cannot touch system files...
        trusted-users = users;
      };
    };
  };

  systems.darwin = {
    # aarch64-darwin can also run x86_64-darwin binaries with Rosetta 2,
    # so we can use the same config for all Darwin systems.
    nix.settings.extra-platforms = ["x86_64-darwin"];
  };
}
