{ lib, ... }:

# let configPath = ../. + builtins.toPath "/easyeffects";
let configPath = ./config;

in {
  services.easyeffects.enable = true;

  xdg.configFile."easyeffects/" = lib.mkIf (builtins.pathExists configPath) {
    source = configPath;
    recursive = true;
  };
}
