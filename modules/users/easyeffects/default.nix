{
  config,
  outputs,
  pkgs,
  ...
}:
# TODO https://github.com/JackHack96/EasyEffects-Presets
# let configPath = ../. + builtins.toPath "/easyeffects";

# NOTE Make sure your headphones are set as default, *not* easyeffects
# NOTE Great set of EQs: https://www.reddit.com/r/oratory1990/wiki/index/list_of_presets/
let
  configPath = ./config;
in
  outputs.lib.mkDesktopModule config "easyeffects" {
    services.easyeffects = {
      enable = true;
      package = pkgs.unstable.easyeffects;
    };

    # https://www.reddit.com/r/oratory1990/wiki/index/list_of_presets/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
    xdg.configFile."easyeffects/" = outputs.lib.mkIf (builtins.pathExists configPath) {
      source = configPath;
      recursive = true;
    };
  }
