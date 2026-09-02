_: {
  flake.homeModules.web = {pkgs, ...}: {
    home.packages = with pkgs; [
      nodejs
      bun
    ];
  };
}
