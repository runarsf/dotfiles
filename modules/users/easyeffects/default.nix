{ config, outputs, ... }:

# let configPath = ../. + builtins.toPath "/easyeffects";
let configPath = ./config;

in outputs.lib.mkDesktopModule config "easyeffects" {
  services.easyeffects.enable = true;

  xdg.configFile."easyeffects/" =
    outputs.lib.mkIf (builtins.pathExists configPath) {
      source = configPath;
      recursive = true;
    };
}
