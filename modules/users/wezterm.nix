{ config, pkgs, inputs, outputs, ... }:

let
  wezterm = inputs.wezterm.packages.${pkgs.system}.default;
  toLua = x:
    if builtins.match ''
      .*
      .*'' x != null then
      outputs.lib.trim x
    else
      "'${x}'";

in outputs.lib.mkDesktopModule' config "wezterm" {
  # FIXME This should be a generic way to start a program, not requiring "start" to be provided
  #  Maybe "exec 'btop'" and "exec' { command = [ 'btop' ], ... }"
  exec = outputs.lib.mkOption {
    type = outputs.lib.types.functionTo outputs.lib.types.str;
    default = { class ? null, command ? [ "start" ] }:
      let
        args = if class == null then
          "${builtins.concatStringsSep " " command}"
        else
          "${builtins.head command} --class ${class} ${
            builtins.concatStringsSep " " (builtins.tail command)
          }";
      in "${outputs.lib.getExe config.programs.wezterm.package} ${args}";
    readOnly = true;
  };
  fonts = outputs.lib.mkOption {
    type = outputs.lib.types.listOf outputs.lib.types.str;
    default = [
      # ''
      # {
      #   family = 'JetBrains Mono',
      #   weight = 'Medium',
      #   harfbuzz_features = { 'calt', 'clig', 'liga', 'ss20', 'cv02', 'cv03', 'cv04', 'cv05', 'cv06', 'cv07', 'cv11', 'cv14', 'cv15', 'cv16', 'cv17' },
      # }
      # ''
      "scientifica"
      "CozetteHiDpi"
      "TamzenForPowerline"
      "Unifont"
      "Unifont Upper"
      "CaskaydiaCove NFM"
    ];
  };
} {
  home.shellAliases.ssh = outputs.lib.mkIf (config.defaultTerminal == "wezterm")
    "TERM=xterm-256color ssh";

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
    extraConfig = let
      tab = ''
        'index',
        { 'process', padding = 0, },
        'ResetAttributes',
        'cwd',
      '';
    in ''
      local wezterm = require 'wezterm'
      local smart_splits = wezterm.plugin.require('https://github.com/mrjones2014/smart-splits.nvim')
      local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")
      local config = wezterm.config_builder()

      tabline.setup({
        options = {
          icons_enabled = true,
          theme = 'Ayu Dark (Gogh)',
          color_overrides = {
            normal_mode = {
              c = { bg = 'none' },
              x = { bg = 'none' },
              tabs = { bg = 'none' },
            },
          },
          section_separators = {
            left = wezterm.nerdfonts.ple_right_half_circle_thick,
            right = wezterm.nerdfonts.ple_left_half_circle_thick,
          },
          component_separators = {
            left = wezterm.nerdfonts.ple_right_half_circle_thin,
            right = wezterm.nerdfonts.ple_left_half_circle_thin,
          },
          tab_separators = {
            left = wezterm.nerdfonts.ple_right_half_circle_thick,
            right = wezterm.nerdfonts.ple_left_half_circle_thick,
          },
        },
        sections = {
          tabline_a = { 'mode' },
          tabline_b = { 'workspace' },
          tabline_c = { ' ' },
          tab_active = {
            ${tab}
            { 'zoomed', padding = 0 },
          },
          tab_inactive = {
            ${tab}
          },
          tabline_x = { 'battery' },
          tabline_y = { 'datetime' },
          tabline_z = { 'hostname' },
        },
        extensions = { },
      })

      tabline.apply_to_config(config)

      smart_splits.apply_to_config(config, {
        direction_keys = { 'LeftArrow', 'DownArrow', 'UpArrow', 'RightArrow' },
        modifiers = {
          move = 'SHIFT',
          resize = 'SHIFT|ALT',
        },
      })

      -- config.color_scheme = 'Ayu Dark (Gogh)'
      -- config.colors = {
      --   background = 'none',
      -- }
      config.font = wezterm.font_with_fallback({
        ${
          builtins.concatStringsSep ",\n  "
          (map toLua config.modules.wezterm.fonts)
        }
      })
      config.font_size = 14.0
      config.warn_about_missing_glyphs = false
      config.default_cursor_style = "SteadyBar"
      config.window_decorations = "NONE"
      config.window_padding = {
        left = 0,
        right = 0,
        top = 0,
        bottom = 0,
      }

      -- config.tab_bar_at_bottom = true
      -- config.hide_tab_bar_if_only_one_tab = true
      -- config.use_fancy_tab_bar = false
      -- config.show_new_tab_button_in_tab_bar = false
      -- config.alternate_buffer_wheel_scroll_speed = 1

      config.set_environment_variables = {
        TERMINFO_DIRS = '${config.home.profileDirectory}/share/terminfo',
        WSLENV = 'TERMINFO_DIRS',
        -- PATH = "${config.home.profileDirectory}/bin:''${PATH}"
      }
      config.term = 'wezterm'

      config.unix_domains = {
        {
          name = 'scratchpad',
        },
      }
      config.launch_menu = {
        {
          args = { 'btop', },
        },
        {
          label = 'Scratchpad',
          domain = { DomainName = 'scratchpad', },
        },
      }

      config.tiling_desktop_environments = {
        'X11 LG3D',
        'X11 bspwm',
        'X11 i3',
        'X11 dwm',
        'Wayland',
      }

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
          key = 'T',
          mods = 'CTRL|SHIFT',
          action = wezterm.action.SpawnCommandInNewTab { cwd = wezterm.home_dir },
        }
      }

      return config
    '';
  };
}
