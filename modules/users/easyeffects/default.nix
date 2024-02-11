{ config, lib, pkgs, ... }:
let configPath = ../. + builtins.toPath "/${config.home.username}/config/easyeffects";
in {
  services.easyeffects.enable = true;

  xdg.configFile."easyeffects/" = lib.mkIf (builtins.pathExists configPath) {
    source = configPath;
    recursive = true;
  };
}
