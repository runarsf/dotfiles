_: {
  flake.homeModules.locales = {pkgs, ...}: {
    home.packages = with pkgs; [
      jq
      yq
      imagemagick
    ];
  };
}
