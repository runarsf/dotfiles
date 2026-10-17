_: {
  flake.homeModules.randomizer = { pkgs, ... }: {
    home.packages = with pkgs; [
      archipelago
      poptracker
    ];
  };
}
