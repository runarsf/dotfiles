_: {
  flake.homeModules.ffxiv = {pkgs, ...}: {
    home.packages = with pkgs; [xivlauncher];
  };
}
