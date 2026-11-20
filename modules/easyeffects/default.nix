_: {
  flake.homeModules.easyeffects = _: {
    # NOTE Make sure your headphones are set as default, *not* easyeffects
    # NOTE Great set of EQs: https://www.reddit.com/r/oratory1990/wiki/index/list_of_presets/
    services.easyeffects.enable = true;

    xdg.configFile."easyeffects/" = {
      source = ./config;
      recursive = true;
    };
  };
}
