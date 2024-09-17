{ config, pkgs, inputs, outputs, ... }:

# TODO Make scratchpad be in a multiplexed domain
#  https://wezfurlong.org/wezterm/config/lua/config/default_domain.html
#  https://wezfurlong.org/wezterm/config/lua/config/default_mux_server_domain.html

let wezterm = inputs.wezterm.packages.${pkgs.system}.default;
in outputs.lib.mkDesktopModule' config "wezterm" {
  modules.wezterm.exe = outputs.lib.mkOption {
    default = outputs.lib.getExe config.programs.wezterm.package;
    type = outputs.lib.types.str;
    readOnly = true;
  };
  modules.wezterm.exec = outputs.lib.mkOption {
    type = outputs.lib.types.functionTo outputs.lib.types.str;
    default = { class ? "scratchpad", command ? "" }:
      "${config.modules.wezterm.exe} start --class ${class} ${command}";
    readOnly = true;
  };
  modules.wezterm.fonts = outputs.lib.mkOption {
    type = outputs.lib.types.listOf outputs.lib.types.str;
    default = [ "scientifica" "CozetteHiDpi" "TamzenForPowerline" "Unifont" "Unifont Upper" "CaskaydiaCove NFM" ];
  };
} {
  nixos = {
    environment.systemPackages = with pkgs; [ egl-wayland ];
    hardware.graphics = {
      enable = true;
      extraPackages = with pkgs; [ egl-wayland ];
    };
  };

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

      -- config.color_scheme = 'Ayu Dark (Gogh)'
      -- config.font = wezterm.font 'Operator Mono Lig'
      config.font = wezterm.font_with_fallback({
        ${
          builtins.concatStringsSep ",\n  "
          (map (font: "'${font}'") config.modules.wezterm.fonts)
        }
      })
      config.font_size = 14.0
      config.warn_about_missing_glyphs = false
      config.default_cursor_style = "SteadyBar"
      config.window_decorations = "NONE"
      config.tab_bar_at_bottom = true
      config.hide_tab_bar_if_only_one_tab = true
      config.show_new_tab_button_in_tab_bar = false
      -- config.alternate_buffer_wheel_scroll_speed = 1
      config.colors = {
        tab_bar = {
          background = 'none',
          active_tab = {
            bg_color = 'none',
            fg_color = 'none',
          },
          inactive_tab = {
            bg_color = 'none',
            fg_color = 'none',
          },
          inactive_tab_hover = {
            bg_color = 'none',
            fg_color = 'none',
          },
          new_tab = {
            bg_color = 'none',
            fg_color = 'none',
          },
          new_tab_hover = {
            bg_color = 'none',
            fg_color = 'none',
          },
        },
      }
      config.window_padding = {
        left = 2,
        right = 0,
        top = 2,
        bottom = 0,
      }

      config.set_environment_variables = {
        TERMINFO_DIRS = '${config.home.profileDirectory}/share/terminfo',
        WSLENV = 'TERMINFO_DIRS',
        PATH = "${config.home.profileDirectory}/bin:''${PATH}"
      }
      config.term = 'wezterm'

      config.tiling_desktop_environments = {
        'X11 LG3D',
        'X11 bspwm',
        'X11 i3',
        'X11 dwm',
        'Wayland',
      }

      local RIGHT_HALF_CIRCLE = wezterm.nerdfonts.ple_right_half_circle_thick
      local LEFT_HALF_CIRCLE = wezterm.nerdfonts.ple_left_half_circle_thick

      function tab_title(tab_info)
        local title = tab_info.tab_title
        if title and #title > 0 then
          return title
        end
        return tab_info.active_pane.title
      end

      wezterm.on(
        'format-tab-title',
        function(tab, tabs, panes, config, hover, max_width)
          local edge_background = 'none'
          local background = '#272C32'
          local foreground = 'none'

          if tab.is_active or hover then
            background = '#CD9446'
            foreground = 'none'
          end

          local edge_foreground = background

          local title = tab_title(tab)

          title = wezterm.truncate_right(title, max_width - 3)

          local bar = {
            { Background = { Color = edge_background } },
            { Foreground = { Color = edge_foreground } },
            { Text = LEFT_HALF_CIRCLE },
            { Background = { Color = background } },
            { Foreground = { Color = foreground } },
            { Text = title },
            { Background = { Color = edge_background } },
            { Foreground = { Color = edge_foreground } },
            { Text = RIGHT_HALF_CIRCLE },
            { Text = " " },
          }

          return bar
        end
      )

      config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
      config.keys = {
        {
          key = 'z',
          mods = 'LEADER',
          action = wezterm.action.TogglePaneZoomState,
        },
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
        {
          key = 'd',
          mods = 'LEADER',
          action = wezterm.action.DetachDomain('CurrentPaneDomain'),
        },
        {
          key = 'LeftArrow',
          mods = 'CTRL|SHIFT',
          action = wezterm.action.ActivateTabRelative(-1),
        },
        {
          key = 'RightArrow',
          mods = 'CTRL|SHIFT',
          action = wezterm.action.ActivateTabRelative(1),
        },
        {
          key = 'c',
          mods = 'ALT',
          action = wezterm.action.CopyTo("Clipboard"),
        },
        {
          key = 'v',
          mods = 'ALT',
          action = wezterm.action.PasteFrom("Clipboard"),
        },
        {
          key = 'LeftArrow',
          mods = 'SHIFT',
          action = wezterm.action.ActivatePaneDirection("Left"),
        },
        {
          key = 'RightArrow',
          mods = 'SHIFT',
          action = wezterm.action.ActivatePaneDirection("Right"),
        },
        {
          key = 'UpArrow',
          mods = 'SHIFT',
          action = wezterm.action.ActivatePaneDirection("Up"),
        },
        {
          key = 'DownArrow',
          mods = 'SHIFT',
          action = wezterm.action.ActivatePaneDirection("Down"),
        },
        {
          key = 'LeftArrow',
          mods = 'SHIFT|ALT',
          action = wezterm.action.AdjustPaneSize { 'Left', 5 },
        },
        {
          key = 'RightArrow',
          mods = 'SHIFT|ALT',
          action = wezterm.action.AdjustPaneSize { 'Right', 5 },
        },
        {
          key = 'UpArrow',
          mods = 'SHIFT|ALT',
          action = wezterm.action.AdjustPaneSize { 'Up', 5 },
        },
        {
          key = 'DownArrow',
          mods = 'SHIFT|ALT',
          action = wezterm.action.AdjustPaneSize { 'Down', 5 },
        },
        {
          key = 'T',
          mods = 'CTRL|SHIFT',
          action = wezterm.action.SpawnCommandInNewTab { cwd = wezterm.home_dir },
        }
      }

      return config
    '';
  };
}
