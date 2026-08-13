_: {
  flake.homeModules.obs = {pkgs, ...}: {
    home.packages = with pkgs; [obs-studio];
  };
}
