{ config, pkgs, inputs, outputs, ... }:

outputs.lib.mkDesktopModule config "wezterm" {
  nixos = {
    environment.systemPackages = with pkgs; [ egl-wayland ];
    hardware.opengl = {
      enable = true;
      extraPackages = with pkgs; [ egl-wayland ];
    };
  };
  programs.wezterm = {
    enable = true;
    package = inputs.wezterm.packages.${pkgs.system}.default;
    extraConfig = ''
      return {
        -- enable_wayland = false,
        -- front_end = "Software",
        -- window_decorations = "None",
        color_scheme = "Ayu Dark (Gogh)",
      }
    '';
    # https://wezfurlong.org/wezterm/config/lua/config/index.html
    # extraConfig = ''
    #   local wezterm = require 'wezterm'

    #   local config = {}

    #   if wezterm.config_builder then
    #     config = wezterm.config_builder()
    #   end

    #   config.color_scheme = 'Ayu Dark (Gogh)'
    #   config.font = 'Operator Mono Lig'

    #   config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
    #   config.keys = {
    #     {
    #       key = '|',
    #       mods = 'LEADER',
    #       action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    #     },
    #     {
    #       key = '-',
    #       mods = 'LEADER',
    #       action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    #     },
    #   }

    #   return config
    # '';
  };
}
