{ config, pkgs, inputs, outputs, ... }:

let wezterm = inputs.wezterm.packages.${pkgs.system}.default;
in outputs.lib.mkDesktopModule' config "wezterm" {
  modules.wezterm.defaultTerminal =
    outputs.lib.mkEnableOption "Use Wezterm as the default terminal";
} {
  home.sessionVariables.TERMINAL =
    outputs.lib.mkIf config.modules.wezterm.defaultTerminal "wezterm";

  nixos = {
    environment.systemPackages = with pkgs; [ egl-wayland ];
    hardware.graphics = {
      enable = true;
      extraPackages = with pkgs; [ egl-wayland ];
    };
  };
  # Yes, this is redundant, but is required because wezterm won't build when only provided as programs.wezterm.package.
  home.packages = [ wezterm ];
  programs.wezterm = {
    enable = true;
    package = wezterm;
    # https://wezfurlong.org/wezterm/config/lua/config/index.html
    extraConfig = ''
      local wezterm = require 'wezterm'

      local config = {}

      if wezterm.config_builder then
        config = wezterm.config_builder()
      end

      config.color_scheme = 'Ayu Dark (Gogh)'
      config.font = wezterm.font 'Operator Mono Lig'
      config.window_decorations = "NONE"
      config.tab_bar_at_bottom = true
      config.hide_tab_bar_if_only_one_tab = true

      config.set_environment_variables = {
        TERMINFO_DIRS = '${config.home.profileDirectory}/share/terminfo',
        WSLENV = 'TERMINFO_DIRS',
        PATH = '${config.home.profileDirectory}/bin:''${PATH}'
      }
      config.term = 'wezterm'

      local RIGHT_HALF_CIRCLE = wezterm.nerdfonts.ple_right_half_circle_thick
      local LEFT_HALF_CIRCLE = wezterm.nerdfonts.ple_left_half_circle_thick

      -- This function returns the suggested title for a tab.
      -- It prefers the title that was set via `tab:set_title()`
      -- or `wezterm cli set-tab-title`, but falls back to the
      -- title of the active pane in that tab.
      function tab_title(tab_info)
        local title = tab_info.tab_title
        -- if the tab title is explicitly set, take that
        if title and #title > 0 then
          return title
        end
        -- Otherwise, use the title from the active pane
        -- in that tab
        return tab_info.active_pane.title
      end

      wezterm.on(
        'format-tab-title',
        function(tab, tabs, panes, config, hover, max_width)
          local edge_background = 'none'
          local background = 'none'
          local foreground = 'none'

          if tab.is_active then
            background = '#CD9446'
            foreground = 'none'
            -- elseif hover then
            --   background = '#DB9E4B'
            --   foreground = 'none'
          end

          local edge_foreground = background

          local title = tab_title(tab)

          -- ensure that the titles fit in the available space,
          -- and that we have room for the edges.
          title = wezterm.truncate_right(title, max_width - 2)

          local result = {
            { Background = { Color = edge_background } },
            { Foreground = { Color = edge_foreground } },
            { Text = LEFT_HALF_CIRCLE },
            { Background = { Color = background } },
            { Foreground = { Color = foreground } },
            { Text = title },
            { Background = { Color = edge_background } },
            { Foreground = { Color = edge_foreground } },
            { Text = RIGHT_HALF_CIRCLE },
          }

          if tab.id > 1 then
            table.insert(result, 1, { Text = " " })
          end

          return result
        end
      )

      config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
      config.keys = {
        {
          key = '|',
          mods = 'LEADER',
          action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
        },
        {
          key = '-',
          mods = 'LEADER',
          action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
        },
      }

      return config
    '';
  };
}
