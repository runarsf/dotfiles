{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "wezterm" {
  programs.wezterm = {
    enable = true;
    package = pkgs.master.wezterm;
    extraConfig = ''
      local wezterm = require 'wezterm'

      local config = {}

      if wezterm.config_builder then
        config = wezterm.config_builder()
      end

      config.color_scheme = 'Ayu Dark (Gogh)'
      -- https://github.com/wez/wezterm/issues/4483
      config.enable_wayland = false

      return config
    '';
  };
}
