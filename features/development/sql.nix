_: {
  flake.homeModules.sql = {pkgs, ...}: {
    home.packages = with pkgs; [
      sqlit-tui
      sqlite
    ];
  };
}
