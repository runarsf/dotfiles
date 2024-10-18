{ config, outputs, pkgs, ... }:

# let configPath = ../. + builtins.toPath "/easyeffects";
let configPath = ./config;

in outputs.lib.mkDesktopModule config "easyeffects" {
  services.easyeffects = {
    enable = true;
    package = pkgs.unstable.easyeffects;
  };

  xdg.configFile."easyeffects/" =
    outputs.lib.mkIf (builtins.pathExists configPath) {
      source = configPath;
      recursive = true;
    };
}
