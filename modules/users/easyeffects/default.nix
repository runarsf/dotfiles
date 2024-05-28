{ lib, ... }:

# let configPath = ../. + builtins.toPath "/easyeffects";
let configPath = ./config;

in {
  # FIXME Crashes when opening preferences, probably an issue with xdg-portals installed
  #  https://github.com/wwmm/easyeffects/issues/1411
  services.easyeffects.enable = true;

  xdg.configFile."easyeffects/" = lib.mkIf (builtins.pathExists configPath) {
    source = configPath;
    recursive = true;
  };
}
